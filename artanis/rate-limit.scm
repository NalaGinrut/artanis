;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

;; =============================================================================
;; Rate limiting with token buckets.
;;
;; A bucket holds at most `capacity' tokens, and is refilled at
;; capacity/period tokens per second, so it allows `capacity' requests per
;; `period' seconds, with a burst of `capacity'. Each request takes a token,
;; a request finding no token is over the limit. The refill is computed when
;; the bucket is used, from a monotonic clock, so there's no timer and no
;; thread, each operation is O(1).
;;
;; A rate limiter has a bucket for each key, e.g. the IP of the client:
;;  - A bucket refilled to full is the same as a new one, so a bucket which
;;    has been idle for `period' is dropped, it changes nothing.
;;  - The keys are limited by max-keys, otherwise a client changing its key
;;    (e.g. its IP) would fill the memory. When there's no room for a new
;;    key, the idle buckets are dropped (at most once per second, so a flood
;;    of new keys doesn't sweep the table each time). If there's still no
;;    room, the new key is over the limit: dropping a busy bucket would reset
;;    its limit, which is what an attacker wants. max-keys should be large
;;    enough that only an attack can fill it.
;;
;; The counts are in the process, multiple instances of a server don't share
;; them.
;; NOTE: A rate limiter is protected by a mutex, since handlers may run in
;;       several threads (server.workers > 1). A bucket alone isn't, it's for
;;       one connection, which is served by one task.
;; =============================================================================

(define-module (artanis rate-limit)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module ((artanis logger) #:select (artanis-log))
  #:use-module ((artanis route) #:select (rc-oht-ref))
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:export (make-token-bucket
            token-bucket?
            token-bucket-take!
            make-rate-limiter
            rate-limiter?
            rate-limiter-take!
            rate-limiter-count
            monotonic-seconds
            rc-rate-limited))

;; Seconds from a monotonic clock, as a real number.
(define (monotonic-seconds)
  (exact->inexact (/ (get-internal-real-time) internal-time-units-per-second)))

;; The idle buckets of a full rate limiter are dropped at most once per this
;; many seconds, and it warns about being full at most once per this.
(define *rate-limiter-interval* 1)
;; The minimum Retry-After in seconds, it's an integer (RFC 9110 10.2.3).
(define *min-retry-after* 1)

(define-record-type token-bucket
  (%make-token-bucket capacity rate tokens last)
  token-bucket?
  (capacity bucket-capacity)
  (rate bucket-rate)                    ; tokens per second
  (tokens bucket-tokens set-bucket-tokens!)
  (last bucket-last set-bucket-last!))  ; the time of the last refill

(define (check-limit who capacity period)
  (unless (and (integer? capacity) (exact? capacity) (> capacity 0))
    (throw 'artanis-err 500 who
           "Invalid capacity `~a', expect a positive integer" capacity))
  (unless (and (real? period) (> period 0))
    (throw 'artanis-err 500 who
           "Invalid period `~a', expect a positive number of seconds" period)))

;; A full bucket allowing capacity requests per period seconds.
(define* (make-token-bucket capacity period #:optional (now (monotonic-seconds)))
  (check-limit 'make-token-bucket capacity period)
  (%make-token-bucket capacity (/ capacity period) capacity now))

(define (refill! b now)
  (let ((elapsed (- now (bucket-last b))))
    (when (> elapsed 0)
      (set-bucket-tokens! b (min (bucket-capacity b)
                                 (+ (bucket-tokens b)
                                    (* elapsed (bucket-rate b)))))
      (set-bucket-last! b now))))

;; Take a token. Returns #t, or the seconds to wait for the next token if
;; there's none, i.e. it's over the limit.
(define* (token-bucket-take! b #:optional (now (monotonic-seconds)))
  (refill! b now)
  (let ((tokens (bucket-tokens b)))
    (cond
     ((>= tokens 1)
      (set-bucket-tokens! b (- tokens 1))
      #t)
     (else (/ (- 1 tokens) (bucket-rate b))))))

;; Is the bucket full at now, i.e. the same as a new one?
(define (bucket-idle? b now)
  (>= (+ (bucket-tokens b) (* (- now (bucket-last b)) (bucket-rate b)))
      (bucket-capacity b)))

(define-record-type rate-limiter
  (%make-rate-limiter capacity period max-keys table count mutex
                      swept-at warned-at)
  rate-limiter?
  (capacity limiter-capacity)
  (period limiter-period)
  (max-keys limiter-max-keys)
  (table limiter-table)
  (count limiter-count set-limiter-count!) ; keys in table
  (mutex limiter-mutex)
  (swept-at limiter-swept-at set-limiter-swept-at!)
  (warned-at limiter-warned-at set-limiter-warned-at!))

;; capacity requests per period seconds for each key.
(define* (make-rate-limiter capacity period
                            #:key (max-keys (get-conf '(server ratelimitkeys))))
  (check-limit 'make-rate-limiter capacity period)
  (unless (and (integer? max-keys) (> max-keys 0))
    (throw 'artanis-err 500 'make-rate-limiter
           "Invalid max-keys `~a', expect a positive integer" max-keys))
  (%make-rate-limiter capacity period max-keys (make-hash-table) 0
                      (make-mutex) #f #f))

;; The number of keys being counted.
(define (rate-limiter-count limiter)
  (with-mutex (limiter-mutex limiter)
    (limiter-count limiter)))

;; At most once per *rate-limiter-interval* since the time in get, then set
;; it.
(define (once-per-interval! limiter get set now)
  (let ((t (get limiter)))
    (and (or (not t) (>= (- now t) *rate-limiter-interval*))
         (begin (set limiter now) #t))))

;; Drop the idle buckets, at most once per *rate-limiter-interval*.
(define (sweep! limiter now)
  (when (once-per-interval! limiter limiter-swept-at set-limiter-swept-at!
                            now)
    (let* ((table (limiter-table limiter))
           (idle (hash-fold (lambda (key b acc)
                              (if (bucket-idle? b now) (cons key acc) acc))
                            '() table)))
      (for-each (lambda (key) (hash-remove! table key)) idle)
      (set-limiter-count! limiter (- (limiter-count limiter) (length idle))))))

(define (new-bucket! limiter key now)
  (let ((max-keys (limiter-max-keys limiter)))
    (when (>= (limiter-count limiter) max-keys)
      (sweep! limiter now))
    (and (< (limiter-count limiter) max-keys)
         (let ((b (make-token-bucket (limiter-capacity limiter)
                                     (limiter-period limiter)
                                     now)))
           (hash-set! (limiter-table limiter) key b)
           (set-limiter-count! limiter (1+ (limiter-count limiter)))
           b))))

;; Take a token for key. Returns #t, or the seconds to wait if it's over the
;; limit, see token-bucket-take!.
(define* (rate-limiter-take! limiter key #:optional (now (monotonic-seconds)))
  (with-mutex (limiter-mutex limiter)
    (let ((b (or (hash-ref (limiter-table limiter) key)
                 (new-bucket! limiter key now))))
      (cond
       (b (token-bucket-take! b now))
       (else
        ;; No room for a new key, it's over the limit.
        (when (once-per-interval! limiter limiter-warned-at
                                  set-limiter-warned-at! now)
          (artanis-log 'warn #f #f
                       #:msg (format #f "Rate limiter is full (~a keys), new keys are rejected~%"
                                     (limiter-max-keys limiter))))
        (limiter-period limiter))))))

;; Is the request of rc over the #:rate-limit of its route? Returns #f, or
;; the seconds to wait for Retry-After (RFC 9110 10.2.3), at least 1.
(define (rc-rate-limited rc)
  (let ((check (rc-oht-ref rc #:rate-limit)))
    (and check
         (let ((r (check rc)))
           (and (not (eq? r #t))
                (max *min-retry-after* (inexact->exact (ceiling r))))))))
