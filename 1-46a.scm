(define (average a b) (/ (+ a b) 2))

(define (iterative-improve good-enough? improver)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improver guess))))
    (iter guess)))

(define tolerance 0.00001)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (* guess guess))) tolerance))
  (define (improver guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improver) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (fixed-point-2 f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (f ((iterative-improve good-enough? f) first-guess)))
