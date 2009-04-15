(require "./main-256")
(load "./path")

(define make-path-test
  (segments->painter
   (make-path (make-vect 0.3 0.3)
              (make-vect 0.7 0.3)
              (make-vect 0.7 0.7)
              (make-vect 0.3 0.7))
   ))

(define make-path-circ-test
  (segments->painter
   (make-path-circ (make-vect 0.3 0.3)
                   (make-vect 0.7 0.3)
                   (make-vect 0.7 0.7)
                   (make-vect 0.3 0.7))
   ))

;(define *painter* make-path-test)
(define *painter* make-path-circ-test)
(main '())

