(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2)) ; (origin edge1 edge2) = (origin . (edge1 . (edge2 . nil)))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))) ; (origin . (edge1 . edge2))

(require "./2-46")
(define v0 (make-vect 1 1))
(define v1 (make-vect 5 4))
(define v2 (make-vect -2 5))
