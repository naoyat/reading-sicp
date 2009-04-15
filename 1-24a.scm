(require "./lib")
(require "./1-21")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;(+ 1 (random (- n 1)) で欲しいのが [1 .. n-1] なのか (1 .. n-1) なのか文意からは不明
; between に両端が含まれるか。テストには含まれないほうがいい気がするが、random の使い方から推測するに、両端を含んだものでテストしているっぽい。
; あと、random の仕様と。
; (random x) が
;   [0 .. x] なら (+ 1 (random (- n 1))) = (+ 1 [0 .. n-1]) = [1 .. n]
;   [0 .. x-1] なら (+ 1 (random (- n 1))) = (+ 1 [0 .. n-2]) = [1 .. n-1] たぶんこれを想定してそう。
;   [1 .. x] なら (+ 1 (random (- n 1))) = (+ 1 [1 .. n-1]) = [2 .. n]
;   [1 .. x-1] なら (+ 1 (random (- n 1))) = (+ 1 [1 .. n-2]) = [2 .. n-1]
; しかし、a=1 だと (expmod 1 n n) があらゆる n(≧2) について 1 になるのでテストにならない。本気だろうか。

(define (random-integer-between a b)
  (+ a (random (+ (- b a) 1))))
(define (random-positive-integer-n-and-less n)
  (+ 1 (random n)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (random-integer-between 2 (- n 2))))

;  (try-it (+ 1 (random (- n 1)))))

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
