(require "./lib")
(require "./1-21")

(define (fast-expt b n)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (define (iter b n product)
    (cond ((= n 0) product)
          ((even? n) (iter (square b) (/ n 2) product))
          (else (iter b (- n 1) (* b product)))))
  (iter b n 1))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;;;;;;;;
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (find-divisor n test-divisor)
  (define (next d)
    (if (= d 2) 3 (+ d 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;
(define (timed-prime-test n times)
  (newline)
  (display n)
  (start-prime-test n times (runtime)))

(define (start-prime-test n times start-time)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;(timed-prime-test 1999)

(define (search-for-primes start end times)
  (define (iter n)
    (timed-prime-test n times)
    (if (< n end)
        (iter (+ n 1))))
  (iter start))

(define (test start end)
  (search-for-primes start end 1))
