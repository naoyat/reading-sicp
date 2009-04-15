(require "./2-59")

(define (reduce-set set)
  (define (iter set result)
    (cond ((null? set) result)
          ((element-of-set? (car set) result)
           (iter (cdr set) result))
          (else
           (iter (cdr set) (cons (car set) result)))))
  (iter set '()))

;(define (reduce-sett set)
;  (union-set set '()))

(define (element-of-set*? x set)
  (element-of-set? x (reduce-set set)))

(define (adjoin-set* x set)
  (adjoin-set x (reduce-set set)))

;(x (o) x)
(define (intersection-set* set1 set2)
  (intersection-set (reduce-set set1)
                    (reduce-set set2)))

;; 2-59
;(o (o) o)
(define (union-set* set1 set2)
  (union-set (reduce-set set1)
             (reduce-set set2)))
