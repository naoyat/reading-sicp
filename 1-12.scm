(define (pascal n k)
  (cond ((or (= k 0) (= k n)) 1)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))
