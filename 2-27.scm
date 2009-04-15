(require "./lib")

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (define (iter rest result)
    (cond ((null? rest) result)
          ((not (pair? (car rest)))
           (iter (cdr rest)
                 (cons (car rest) result)))
          (else
           (iter (cdr rest)
                 (cons (deep-reverse (car rest)) result)))
           ))
  (iter x nil))
