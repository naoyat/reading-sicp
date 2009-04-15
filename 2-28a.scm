(define nil '())
(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest)
              (append result ((if (pair? (car rest)) fringe list) (car rest))))))
  (iter tree nil))
