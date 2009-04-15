(require "./lib")
(require "./1-21")

(define (test n) (start-test (lambda () (prime? n))))
(define (start-test f)
  (let ((start-time (runtime))
        (rep 10000))
    (loop-test f rep)
    (/ (- (runtime) start-time) rep)))
(define (loop-test f rep)
  (define (iter c)
    (f)
    (if (> c 0) (iter (- c 1))))
  (iter rep))

;;;
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;(timed-prime-test 1999)

(define (search-for-primes start end)
  (define (iter n)
    (timed-prime-test n)
    (if (< n end)
        (iter (+ n 1)))
    )
  (iter start))
