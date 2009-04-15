(require "./1-43")

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smoothed- f n)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (smooth result))))
  (iter n f))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

(define (square x) (* x x))
(define (inc x) (+ x 1))

