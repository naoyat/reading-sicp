(require "./lib")

(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))
