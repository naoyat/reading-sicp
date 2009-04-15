(require "./2-78")

(define (install-zero-package)
  (define (=zero-scheme-number? n)
    (= n 0)) ;; (zero? n)

  (define (numer x) (car x))
; (define (denom x) (cdr x))
  (define (=zero-rational? r)
    (= (numer r) 0)) ; denom==0 ¤Ê¤é NaN

  (define (=zero-complex? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  ;
  (put '=zero? 'scheme-number =zero-scheme-number?)
  (put '=zero? 'rational =zero-rational?)
  (put '=zero? 'complex =zero-complex?)

  'done)

(define (=zero? x)
  (let ((proc (get '=zero? (type-tag x))))
    (if proc
        (proc (contents x))
        (error "not supported for this type"
               (list '=zero? (type-tag x))))
        ))

;;
;;TEST
;;
(install-zero-package)
(=zero? 1) ; #f
(=zero? 0) ; #t

(=zero? (make-rational 1 1)) ; #f
(=zero? (make-rational 0 1)) ; #t

(=zero? (make-complex-from-real-imag 1 0)) ; #f
(=zero? (make-complex-from-real-imag 0 1)) ; #f
(=zero? (make-complex-from-real-imag 1 1)) ; #f
(=zero? (make-complex-from-real-imag 0 0)) ; #t
(=zero? (make-complex-from-mag-ang 1 0)) ; #f
(=zero? (make-complex-from-mag-ang 0 1)) ; #t !!!
(=zero? (make-complex-from-mag-ang 1 1)) ; #f
(=zero? (make-complex-from-mag-ang 0 0)) ; #t

(define (equ-using-zero? x y) (=zero? (sub x y)))

(equ-using-zero? 1 3) ; #f
(equ-using-zero? 3 3) ; #t

(equ-using-zero? (make-rational 1 2) (make-rational 2 3)) ; #f
(equ-using-zero? (make-rational 1 2) (make-rational 2 4)) ; #t

(equ-using-zero? (make-complex-from-real-imag 2 2) (make-complex-from-real-imag 2 2)) ; #t
(equ-using-zero? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 5)) ; #f
(equ-using-zero? (make-complex-from-real-imag 5 0) (make-complex-from-mag-ang 5 0)) ; #t
