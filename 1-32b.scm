(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))
        ))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (inc x) (+ x 1))
(define (as-is x) x)


(define (pi-sum n)
  (* 8 (sum (lambda (x) 
              (/ 1.0 (* (- (* 4 x) 1) 
                        (- (* 4 x) 3))))
            1
            inc
            n)))

(define (pi-product n)
  (* 4 (product (lambda (x)
                  (if (even? x)
                      (/ (+ x 2) (+ x 1))
                      (/ (+ x 1) (+ x 2))))
                1
                inc
                n)))
