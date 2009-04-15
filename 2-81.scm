(require "./c252")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;;a
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (expt x y)))
;     (lambda (x y) (tag (expt x y))))

(exp 3 3) ; 27
(exp 2 10) ; 1024

(define z (make-complex-from-real-imag 1 2))
(exp z z) ; loop
(exp z 2) ; loop
(exp 2 z) ; loop

