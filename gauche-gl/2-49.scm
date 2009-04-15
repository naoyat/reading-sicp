(require "./main-256")
(load "./path")

(define frame-rectangle
  (segments->painter
   (make-path-circ (make-vect 0.0 0.0)
                   (make-vect 1.0 0.0)
                   (make-vect 1.0 1.0)
                   (make-vect 0.0 1.0))
   ))

(define letter-x
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0)
                  (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 0.0)
                  (make-vect 0.0 1.0)))
   ))

(define lozenge
  (segments->painter
   (make-path-circ (make-vect 0.5 0.0)
                   (make-vect 1.0 0.5)
                   (make-vect 0.5 1.0)
                   (make-vect 0.0 0.5))
   ))

(define wave
  (segments->painter
   (append
    (make-path (make-vect 0.0  0.86)
               (make-vect 0.16 0.60)
               (make-vect 0.28 0.65)
               (make-vect 0.42 0.65)
               (make-vect 0.35 0.86)
               (make-vect 0.42 1.0))
    (make-path (make-vect 0.58 1.0)
               (make-vect 0.65 0.86)
               (make-vect 0.58 0.65)
               (make-vect 0.76 0.65)
               (make-vect 1.0  0.35))
    (make-path (make-vect 1.0  0.14)
               (make-vect 0.60 0.46)
               (make-vect 0.76 0.0))
    (make-path (make-vect 0.58 0.0)
               (make-vect 0.50 0.17)
               (make-vect 0.42 0.0))
    (make-path (make-vect 0.24 0.0)
               (make-vect 0.35 0.51)
               (make-vect 0.30 0.59)
               (make-vect 0.16 0.41)
               (make-vect 0.0  0.65))
    )))

;(define *painter* frame-rectangle)
;(define *painter* letter-x)
;(define *painter* lozenge)
(define *painter* wave)
(main '())
