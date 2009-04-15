(define (my-good-enough? guess last-guess); x)
  (< (/ (abs (- guess last-guess)) last-guess) 0.00001))

(define (newton-iter guess last-guess x improve-func)
  (if (my-good-enough? guess last-guess)
      guess
      (newton-iter (improve-func guess x) guess x improve-func)))

(use slib)
(require 'trace)

(define (newton x improve-func)
  (newton-iter 1.0 x x improve-func))

(define (sqrt x)
  (newton x
          (lambda (guess x) (/ (+ guess (/ x guess)) 2))
          ))

(define (cube-root x)
  (newton x
          (lambda (guess x) (/ (+ (/ x guess guess) (* guess 2)) 3))
          ))

