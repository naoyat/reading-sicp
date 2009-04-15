(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fast-expr b n)
  (define (iter a b n)
    (print a " " b " " n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))