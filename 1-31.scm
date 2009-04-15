(define (product* term a next b)
  (if (> a b)
      1
      (* (term a)
         (product* term (next a) next b))))

(define (factorial* f a b)
  (define (next x) (+ x 1))
  (product* f a next b))

(define (pi* n)
  (define (f k)
    (if (odd? k)
        (/ (+ k 1) (+ k 2))
        (/ (+ k 2) (+ k 1))))
  (* 4 (factorial* f 1 n)))


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
     result
     (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial f a b)
  (define (next x) (+ x 1))
  (product f a next b))
;  (* (product f (+ a (/ dx 2.0)) next b)
;     dx))

(define (pi n)
  (define (f k)
    (if (odd? k)
        (/ (+ k 1) (+ k 2))
        (/ (+ k 2) (+ k 1))))
  (* 4 (factorial f 1 n)))

