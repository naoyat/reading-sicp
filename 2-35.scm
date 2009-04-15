(require "./lib")

(define (count-leaves-sub tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (count-leaves tree))))

;(define (cl-sub tree)
;  (cond ((null? tree) 0)
;       ((not (pair? tree)) 1)
;        (else (cl tree))))
;(define (cl t)
;  (accumulate (lambda (x y) (+ (cl-sub x) (cl y))) nil t))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (tree)
                         (cond ((null? tree) 0)
                               ((not (pair? tree)) 1)
                               (else (count-leaves tree)))) t)))


(define x (cons (list 1 2) (list 3 4))) ; ((1 2) 3 4)
(define y (list (list 1 2) (list 3 4))) ; ((1 2) (3 4))


