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

(define-module (artanis security passwd)
  #:use-module (artanis utils)
  #:use-module (artanis ffi)
  #:use-module (system foreign)
  #:use-module ((rnrs)
                #:select (bytevector?
                          make-bytevector bytevector-length
                          bytevector=? bytevector-copy
                          bytevector-u8-set!
                          define-record-type))
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11) ; for let-values
  #:export (artanis-passwd-hash
            artanis-passwd-verify))

;; Argon2 constants
(define ARGON2_OK 0)
(define ARGON2I 1)
(define ARGON2ID 2)
(define ARGON2_VERSION_13 #x13)

;; Default parameters (recommended values)
(define *default-t-cost* 2)        ; number of iterations
(define *default-m-cost* 65536)    ; memory usage in KiB (64 MiB)
(define *default-parallelism* 1)   ; number of threads
(define *default-hashlen* 32)      ; hash output length

;; NOTE: For most cases, 16 bytes (128 bits) of salt is sufficient.
(define *saltlen* 16)

(eval-when (eval load compile)
  (ffi-binding "libargon2"
    ;; Core hashing functions
    (define-c-function int argon2i_hash_encoded
      (uint32 uint32 uint32 '* size_t '* size_t size_t '* size_t))

    (define-c-function int argon2id_hash_encoded
      (uint32 uint32 uint32 '* size_t '* size_t size_t '* size_t))

    (define-c-function int argon2_hash
      (uint32 uint32 uint32 '* size_t '* size_t '* size_t '* size_t int uint32))
    (define-c-function int argon2_encodedlen (uint32 uint32 uint32 size_t size_t int))

    ;; Verification functions
    (define-c-function int argon2i_verify ('* '* size_t))

    (define-c-function int argon2id_verify ('* '* size_t))

    ;; Error handling
    (define-c-function '* argon2_error_message (int))))

;; Error message helper
(define (argon2:error-message error-code)
  (pointer->string (%argon2_error_message error-code)))

;; Check return value and throw error if needed
(define-syntax-rule (check-argon2-result ret proc-name)
  (unless (= ret ARGON2_OK)
    (throw 'artanis-err 500 proc-name
           "Argon2 error: ~a" (argon2:error-message ret))))

;; =========================
;; Internal helpers
;; =========================

(define (->bytevector x)
  (if (string? x)
      (string->bytevector x "utf-8")
      x))

(define (variant->type v)
  (case v
    ((argon2i) ARGON2I)
    ((argon2id) ARGON2ID)
    (else
     (throw 'artanis-err 500 'argon2
            "Invalid variant: ~a (use 'argon2id or 'argon2i)" v))))

(define (variant->hash-fn v)
  (case v
    ((argon2i) %argon2i_hash_encoded)
    ((argon2id) %argon2id_hash_encoded)
    (else
     (throw 'artanis-err 500 'argon2
            "Invalid variant: ~a" v))))

(define (variant->verify-fn v)
  (case v
    ((argon2i) %argon2i_verify)
    ((argon2id) %argon2id_verify)
    (else
     (throw 'artanis-err 500 'argon2
            "Invalid variant: ~a" v))))

(define (compute-encoded-length t-cost m-cost parallelism saltlen hashlen variant)
  (%argon2_encodedlen t-cost m-cost parallelism
                      saltlen hashlen
                      (variant->type variant)))

(define (generate-salt)
  (get-random-from-dev #:length *saltlen* #:bv? #t))

(define* (argon2:hash-password password
                               #:key
                               (t-cost *default-t-cost*)
                               (m-cost *default-m-cost*)
                               (parallelism *default-parallelism*)
                               (hashlen *default-hashlen*)
                               (salt (generate-salt))
                               (variant 'argon2id))
  "Secure default Argon2 hash → returns encoded string"
  (let* ((pwd-bv (->bytevector password))
         (pwd-ptr (bytevector->pointer pwd-bv))
         (pwd-len (bytevector-length pwd-bv))

         (salt-bv (->bytevector salt))
         (salt-ptr (bytevector->pointer salt-bv))
         (salt-len *saltlen*)

         (encoded-len (compute-encoded-length
                       t-cost m-cost parallelism
                       salt-len hashlen variant))
         (encoded (make-bytevector encoded-len 0))
         (encoded-ptr (bytevector->pointer encoded))

         (hash-fn (variant->hash-fn variant)))

    (let-values (((ret errno)
                  (hash-fn t-cost m-cost parallelism
                           pwd-ptr pwd-len
                           salt-ptr salt-len
                           hashlen
                           encoded-ptr encoded-len)))
      (check-argon2-result ret 'argon2:hash-password)
      (pointer->string encoded-ptr))))

(define* (argon2:verify-password encoded password
                                 #:key (variant 'argon2id))
  (let* ((encoded-ptr (string->pointer encoded))
         (pwd-bv (->bytevector password))
         (pwd-ptr (bytevector->pointer pwd-bv))
         (pwd-len (bytevector-length pwd-bv))
         (verify-fn (variant->verify-fn variant)))
    (let-values (((ret errno)
                  (verify-fn encoded-ptr pwd-ptr pwd-len)))
      (= ret ARGON2_OK))))

(define (argon2i:hash-encoded t-cost m-cost parallelism pwd salt hashlen)
  (argon2:hash-password pwd
                        #:t-cost t-cost
                        #:m-cost m-cost
                        #:parallelism parallelism
                        #:hashlen hashlen
                        #:salt salt
                        #:variant 'argon2i))

(define (argon2id:hash-encoded t-cost m-cost parallelism pwd salt hashlen)
  (argon2:hash-password pwd
                        #:t-cost t-cost
                        #:m-cost m-cost
                        #:parallelism parallelism
                        #:hashlen hashlen
                        #:salt salt
                        #:variant 'argon2id))

(define (argon2i:verify encoded pwd)
  (argon2:verify-password encoded pwd #:variant 'argon2i))

(define (argon2id:verify encoded pwd)
  (argon2:verify-password encoded pwd #:variant 'argon2id))

(define-record-type argon2-config
    (fields
     t-cost m-cost parallelism hashlen variant))

(define current-argon2-config
  (make-parameter
   (make-argon2-config
    *default-t-cost*
    *default-m-cost*
    *default-parallelism*
    *default-hashlen*
    'argon2id)))

(define (artanis-passwd-hash pwd)
  (let ((argon-config (current-argon2-config)))
    (argon2:hash-password pwd
                          #:t-cost (argon2-config-t-cost argon-config)
                          #:m-cost (argon2-config-m-cost argon-config)
                          #:parallelism (argon2-config-parallelism argon-config)
                          #:salt (generate-salt)
                          #:variant 'argon2id
                          #:hashlen (argon2-config-hashlen argon-config))))

(define (artanis-passwd-verify encoded passwd)
  (let ((argon-config (current-argon2-config)))
    (argon2:verify-password encoded passwd
                            #:variant (argon2-config-variant argon-config))))

(define (init-passwd-policy!)
  #t  )
