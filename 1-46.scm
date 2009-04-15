(define (average a b) (/ (+ a b) 2))

(define (iterative-improve sufficient? improver)
  (lambda (guess)
    (define (iter guess)
      (if (sufficient? guess)
          guess
          (iter (improver guess))))
    (iter guess)))

(define tolerance 0.00001)

(define (sqrt-ii x)
  (define (sufficient? guess)
    (< (abs (- x (* guess guess))) tolerance))
  (define (improver guess)
    (average guess (/ x guess)))
  ((iterative-improve sufficient? improver) 1.0))

(define (fixed-point-ii f first-guess)
  (define (sufficient? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve sufficient? f) first-guess))

(define (fixed-point-ij f first-guess)
  (define (sufficient? guess)
    (< (abs (- guess (f guess))) tolerance))
  (f ((iterative-improve sufficient? f) first-guess)))
