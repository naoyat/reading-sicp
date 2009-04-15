
(define (expt a n)
  (define (iter a i p)
    (if (= i 0)
        p
        (iter a (- i 1) (* p a))
        ))
  (iter a n 1))

(define (cons23 x y) (* (expt 2 x) (expt 3 y)))
(define (car23 z) (part z 2))
(define (cdr23 z) (part z 3))

(define (prime? n) #t)

(define (part x p)
  (define (iter x n)
    (if (or (= x 1)
            (not (= (remainder x p) 0)))
        n
        (iter (/ x p) (+ n 1))))
  (if (prime? p)
      (iter x 0)
      0))

