(require "./pictlang")

(define (split divisor1 divisor2)
  (lambda (painter n)
    (define (self painter n)
      (if (= n 0)
          painter
          (let ((smaller (self painter (- n 1))))
            (divisor1 painter (divisor2 smaller smaller)))))
    (self painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(define *painter* (corner-split wave 4))
(main '())
