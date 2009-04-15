;(define (make-vect a b) (cons a b))
;(define (make-segment a b) (cons a b))
;(define (segments->painter x) x)
(require "./path")

(define wave-segments
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
   ))

(define wave-smile-segments
  (append wave-segments
          (list
           ;; left eye
           (make-segment (make-vect 0.43 0.86)
                         (make-vect 0.48 0.88))
           ;; right eye
           (make-segment (make-vect 0.52 0.88)
                         (make-vect 0.57 0.86))
           )
          ;; mouth
          (make-path (make-vect 0.43 0.74)
                     (make-vect 0.50 0.71)
                     (make-vect 0.57 0.74))
          ))

(define wave (segments->painter wave-segments))
(define wave-smile (segments->painter wave-smile-segments))
