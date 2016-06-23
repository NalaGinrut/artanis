(define (coroutine-1)
  (display "Accepted request 1\n")
  (display "Processing request 1\n")
  ;; If EWOULDBLOCK
  (coroutine-sleep 1)
  ;; Resumed when condition is met
  (display "Continue request 1\n")
  (display "End coroutine-1\n"))

(define (coroutine-2)
  (display "Accepted request 2\n")
  (display "Processing request 2\n")
  ;; If EWOULDBLOCK
  (coroutine-sleep 2)
  ;; Resume
  (display "Continue request 2\n")
  ;; EWOULDBLOCK again
  (coroutine-sleep 2)
  ;; Resume
  (display "End coroutine-2\n"))

(define (run)
  (spawn (coroutine-1))
  (spawn (coroutine-2))
  (schedule))
