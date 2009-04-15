(define (double proc)
  (lambda (x) (proc (proc x))))

(define (inc x) (+ x 1))
