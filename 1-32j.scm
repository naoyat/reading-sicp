(require "./1-32")

(define (accumulate combiner null-value term a next b)
  (define (iter accumulation-of-preceding-terms current-term)
    (if (> current-term b)
        accumulation-of-preceding-terms
        (iter (combiner accumulation-of-preceding-terms (term current-term))
              (next current-term))))
  (iter null-value a))
