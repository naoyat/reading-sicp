(require "./lib")

(define x (list (list 1 2) (list 3 4)))
(define y '(1 (2 (3) 4) (5 (6 7) 8) 9))

(define (deep-reverse x)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest)
              (cons (if (pair? (car rest))
                        (deep-reverse (car rest))
                        (car rest))
                    result))))
  (iter x nil))
