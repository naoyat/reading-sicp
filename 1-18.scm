(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (even? n) (= (remainder n 2) 0))

(define (mul17 a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul17 a (halve b))))
        (else (+ a (mul17 a (- b 1))))))

(define (mul18 a b)
  (define (iter a b product)
    (cond ((= b 0) product)
          ((even? b) (iter (double a) (halve b) product))
          (else (iter a (- b 1) (+ a product)))))
  (iter a b 0))
