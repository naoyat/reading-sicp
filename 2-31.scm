(require "./lib")

(define (square-tree tree) (tree-map square tree))

(define (tree-map func tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map func (car tree))
                    (tree-map func (cdr tree))))))
             
(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;(define sub (lambda (sub-tree)
;         (cond
;          ((null? sub-tree) nil)
;          ((not (pair? sub-tree)) (func sub-tree))
;          (else (tree-map sub-tree))))
;  )