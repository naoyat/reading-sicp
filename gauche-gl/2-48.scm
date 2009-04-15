(require "./2-47")

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
