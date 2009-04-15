(define (pascal n k)
  (if (or (= k 1) (= k n))
      1
      (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k))))
