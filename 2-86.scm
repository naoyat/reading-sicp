; complex回りを変更
(require "./2-77")
(require "./2-84") ;fit-args, higher
(require "./2-85") ;drop, equ? ; 2-84, equ
; なにか、2-85 で上書きされたものが悪さをしている...って apply generic か

(define (scheme-number->integer x) (make-integer (round x)))
(define (integer->scheme-number x) (contents x))
(put 'raise 'scheme-number scheme-number->integer)
(put 'project 'integer integer->scheme-number)
;(define (scheme-number->real x) (make-real x))
;(define (real->scheme-number x) (contents x))
;(put 'project 'scheme-number scheme-number->real)
;(put 'raise 'real real->scheme-number)

(define (higher-definition)
  (let ((type-list '(complex real rational integer scheme-number)))
;  (let ((type-list '(real rational integer scheme-number)))
    (lambda (t1 t2)
      (if (memq t2 (cdr (memq t1 type-list)))
          #t
          #f))))

(define higher? (higher-definition))

(define (fit-args args)
;  (print "FIT-ARGS " (car args) " AND " (cadr args))
  (let ((a1 (car args))
        (a2 (cadr args)))
    (let ((t1 (type-tag a1))
          (t2 (type-tag a2)))
      (if (eq? t1 t2)
          args
          (if (higher? t1 t2)
              (fit-args (list a1 (raise a2)))
              (fit-args (list (raise a1) a2)))
              ))))

;(require "./put-get")
;(require "./tag")
(define (raise-to-real x)
;  (print "RAISE " (type-tag x) " TO REAL")
  (car (fit-args (list x (make-real 1)))))

(define (apply-in-real proc . arg)
  (let ((argc (length arg)))
    (cond ((= argc 1)
           (let ((r (raise-to-real (car arg))))
             (attach-tag 'real (proc (contents r)))))
          ((= argc 2)
           (let ((u (raise-to-real (car arg)))
                 (v (raise-to-real (cadr arg))))
             (attach-tag 'real (proc (contents u) (contents v)))))
          (else
           #f))))

(define (sine x) (apply-in-real sin x))
(define (cosine x) (apply-in-real cos x))
(define (sqrt* x) (apply-in-real sqrt x))
(define (atan* y x) (apply-in-real atan y x))
(define (square* x) (apply-in-real square x)) ; (mul x x)

(put 'add '(real real) (lambda (x y) (make-real (+ x y))))
(put 'sub '(real real) (lambda (x y) (make-real (- x y))))
(put 'mul '(real real) (lambda (x y) (make-real (* x y))))
(put 'div '(real real) (lambda (x y) (make-real (/ x y))))

(define (update-rectangular-package)
  ;;
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt* (add (square* (real-part z))
                (square* (imag-part z)))))
  (define (angle z)
    (atan* (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'updated)

(define (update-polar-package)
  ;;
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt* (add (square* x) (square* y)))
          (atan* y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'updated)

;(install-rectangular-package)
;(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;(require "./complex")
(define (update-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2)) ;+ のかわりに add
                         (add (imag-part z1) (imag-part z2)))) ;+ のかわりに add
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  ;;
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  'updated)


;;
;;
;;
(update-rectangular-package)
(update-polar-package)
(update-complex-package)

;;;;
;;;;
(define z (make-complex-from-real-imag 3 4))
(define z1 (make-complex-from-real-imag (make-rational 2 3) 5))
(define z2 (make-complex-from-mag-ang 1 (make-rational 1 2)))

;(real-part z1)
;(put 'real-part '(complex) real-part)
;(put 'imag-part '(complex) imag-part)
;(put 'magnitude '(complex) magnitude)
;(put 'angle '(complex) angle)

;; patch
(define (complex->real x)
  (let ((rp (real-part x)))
    (if (pair? rp)
        (raise-to-real rp)
        (make-real rp))))
(put 'project 'complex complex->real)

(define (equ-complex? z1 z2)
  (and (equ? (real-part z1) (real-part z2))
       (equ? (imag-part z1) (imag-part z2))))
(put 'equ? '(complex complex) equ-complex?)
