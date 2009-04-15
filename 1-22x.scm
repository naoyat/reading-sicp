(require "./lib")
(require "./1-21")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

;(timed-prime-test 1999)

(define (search-for-primes start)
  (define (iter n cnt)
    (if (= cnt 0)
        (newline);end
        (iter (+ n 1)
              (if (timed-prime-test n) (- cnt 1) cnt))
        ))
  (iter start 3))
