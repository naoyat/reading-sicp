(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (simpson* h)
    (define (y k) (f (+ a (* k h))))
    (define (mag k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            ((odd? k) 4)
            (else 2)))
    (define (term k)
      (* (mag k) (y k)))
    (Define (inc i) (+ i 1))
    (/ (* h (sum term
                 0
                 inc
                 n)) 3))
  (simpson* (/ (- b a) n)))

(define (simpson-let f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k) (f (+ a (* k h))))
    (define (mag k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            ((odd? k) 4)
            (else 2)))

    (/ (* h (sum (lambda (k) (* (mag k) (y k)))
                 0
                 (lambda (i) (+ i 1))
                 n)) 3)))
