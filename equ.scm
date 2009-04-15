; (require "./2-78")

(define (install-equ-package)
  (define (equ-scheme-number? n1 n2)
    (= n1 n2))

  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equ-rational? r1 r2)
    (and (= (numer r1) (numer r2))
         (= (denom r1) (denom r2))))

  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;
  (put 'equ? '(scheme-number scheme-number) equ-scheme-number?)
  (put 'equ? '(rational rational) equ-rational?)
  (put 'equ? '(complex complex) equ-complex?)

  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))

  'done)

(define (equ? x y) (apply-generic 'equ? x y))
;(define (equ-using-zero? x y) (=zero? (sub x y)))

;;
;;TEST
;;
(install-equ-package)
(equ? 1 3) ; #f
(equ? 3 3) ; #t

(equ? (make-rational 1 2) (make-rational 2 3)) ; #f
(equ? (make-rational 1 2) (make-rational 2 4)) ; #t

(equ? (make-complex-from-real-imag 2 2) (make-complex-from-real-imag 2 2)) ; #t
(equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 5)) ; #f
(equ? (make-complex-from-real-imag 5 0) (make-complex-from-mag-ang 5 0)) ; #t
