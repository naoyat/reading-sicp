(define nil '())
(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define (iter rest result)
    (cond ((null? rest) result)
          ((not (pair? (car rest)))
           (iter (cdr rest) (append result (list (car rest)))))
          (else
           (iter (cdr rest) (append result (fringe (car rest)))))))
  (iter tree nil))