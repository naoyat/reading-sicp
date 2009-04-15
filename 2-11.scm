(require "./2-7")

(define (mul-interval-ben x y)
  (cond ((<= 0 (lower-bound x)) ; X=Àµ
         (cond ((<= 0 (lower-bound y)) ; Y=Àµ
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((<= (upper-bound y) 0) ; Y=Éé
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))
               (else ; Y=¸Ù
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ))
        ((<= (upper-bound x) 0) ; X=Éé
         (cond ((<= 0 (lower-bound y)) ; Y=Àµ
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (lower-bound y))))
               ((<= (upper-bound y) 0) ; Y=Éé
                (make-interval (* (upper-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))
               (else ; Y=¸Ù
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))
               ))
        (else ; X=¸Ù
         (cond ((<= 0 (lower-bound y)) ; Y=Àµ
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((<= (upper-bound y) 0) ; Y=Éé
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))
               (else ; Y=¸Ù
                (make-interval (min (* (lower-bound x) (upper-bound y))
                                    (* (upper-bound x) (lower-bound y)))
                               (max (* (lower-bound x) (lower-bound y))
                                    (* (upper-bound x) (upper-bound y)))))
               ))
        ))

;; sample
(define x+ (make-interval 2 11))
(define x- (make-interval -7 -3))
(define x+- (make-interval -5 13))

