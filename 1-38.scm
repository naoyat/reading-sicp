(require "./1-37")

(define (e-euler n)
  (+ 2 (cont-frac (lambda (i)
                    1.0)
                  (lambda (i)
                    (if (= (remainder i 3) 2)
                        (* (+ 1.0 i) 2/3)
                        1.0))
                  (* 3 n))))

(test-from-a-to-b e-euler 1 10)
