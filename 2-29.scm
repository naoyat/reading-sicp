(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure)) ; length=数, structure=数||別のモビール

;; sample
(define m (make-mobile
           (make-branch 5 100)
           (make-branch 10 (make-mobile
                            (make-branch 1 40)
                            (make-branch 4 10)))))
;; a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;; b
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; c
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))
