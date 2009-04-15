(require "./pictlang")

(require "./wave")
(require "./sei")

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-limit-x painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

(define (square-limit-rev painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

;(define *painter* (square-limit wave-smile 4))
;(main '())

(define (right-split painter n)
  (if (= n 0)
      painter
      (beside painter (right-split painter (- n 1)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter (up-split painter (- n 1)))))

;(define *painter* (square-limit sei 4))
;(define *painter* (square-limit wave 4))
;(define *painter* (square-limit-x wave 4))
;(define *painter* (square-limit-rev wave 4))
;(define *painter* (square-limit wave-smile 4))
(define *painter* wave-smile)
(main '())
