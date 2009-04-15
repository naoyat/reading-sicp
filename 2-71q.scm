(define (rate n)
  (define (sum-up base n result)
    (if (= n 0)
        result
;        (/ result (- base 1))
        (sum-up base (- n 1) (+ (/ result 2) n)
;        (sum-up (* base 2) (- n 1) (+ result (* base n)))
                ))
  (sum-up 2 (- n 1) (- n 1)))

