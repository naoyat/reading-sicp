(require "./1-32")

(define (accumulate combiner null-value term a next b)
  (define (iter accumulation-of-preceding-terms current-term)
    (if (> current-term b)
        (combiner accumulation-of-preceding-terms null-value)
        (iter (combiner accumulation-of-preceding-terms (term current-term))
              (next current-term))))
  (if (> a b)
      null-value
      (iter (term a) (next a))))
