(require "./2-7")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define x (make-interval 9.5 10.5)) ; 10¡Ş5%
(define y (make-interval 4.9 6))
(define z (make-interval 2 2.5))

(define a (make-interval 0.1 0.2))
(define b (make-interval 0.015 0.025))
(define c (make-interval 0.0003 0.0004))

(define (fn1 r1 r2)
  (div-interval (add-interval r1 r2) (mul-interval r1 r2)))

(define (fn2 r1 r2)
  (let ((one (make-interval 1 1)))
    (add-interval (div-interval one r1)
                  (div-interval one r2))))
                  