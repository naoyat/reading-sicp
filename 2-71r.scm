(define (rate n)
  (define (sum-up n result)
    (if (= n 0)
        (/ result 2)
        (sum-up (- n 1) (+ (/ result 2) n))
                ))
  (sum-up (- n 1) (/ (- n 1) 2))

