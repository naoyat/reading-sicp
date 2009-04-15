;(define (make-vect a b) (cons a b))
;(define (make-segment a b) (cons a b))
;(define (segments->painter x) x)
;(require "./path")

(define sei
  (segments->painter
   (list
    (make-segment (make-vect 0.2 0.8)
                  (make-vect 0.8 0.8))
    (make-segment (make-vect 0.5 0.8)
                  (make-vect 0.5 0.2))
    (make-segment (make-vect 0.5 0.5)
                  (make-vect 0.7 0.5))
    (make-segment (make-vect 0.3 0.5)
                  (make-vect 0.3 0.2))
    (make-segment (make-vect 0.2 0.2)
                  (make-vect 0.8 0.2)))
   ))


