(define (my-good-enough? guess last-guess); x)
  (< (/ (abs (- guess last-guess)) last-guess) 0.00001))

(define (improve-cube guess x)
  (/ (+ (/ x guess guess) (* guess 2)) 3))

(define (cube-root-iter guess last-guess x)
  (if (my-good-enough? guess last-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess
                       x)))
(define (cube-root x)
  (cube-root-iter 1.0 x x))

(use slib)
(require 'trace)
(trace my-good-enough?)
(trace improve-cube)
(trace cube-root-iter)


