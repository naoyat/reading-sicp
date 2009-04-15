(require "./lib")

(define x '(1 2 3 4))

(define (reverse x)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest) (cons (car rest) result))))
  (iter x nil))
