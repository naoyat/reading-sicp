(require "./pictlang")

;; Q2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;(define *painter* (corner-split wave 4))
(define *painter* (up-split wave 4))
(main '())
