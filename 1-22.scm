(require "./lib")
(require "./1-21")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
;      (time->seconds (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;(timed-prime-test 1999)

(define (search-for-primes start end)
  (define (iter n)
    (timed-prime-test n)
    (if (< n end)
        (iter (+ n 1))))
  (iter start))
