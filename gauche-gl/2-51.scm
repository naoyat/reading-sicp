;(require "./main-256")
(require "./pictlang")
;(require "./2-50") ;rotate

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0))) ; ちゃんと使われてない。あんまり意味ない
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5))) ; ちゃんと使われてない。あんまり意味ない
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

(require "./wave")
(require "./sei")

;(define *painter* (below1 sei wave))
(define *painter* (below2 sei wave))
(main '())
