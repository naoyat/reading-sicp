; complex回りを変更
(require "./2-77")
(require "./2-84") ;fit-args, higher
(require "./2-85") ;drop, equ? ; 2-84, equ
; なにか、2-85 で上書きされたものが悪さをしている...って apply generic か

(define (scheme-number->integer x) (make-integer (round x)))
(define (integer->scheme-number x) (contents x))
(put 'raise 'scheme-number scheme-number->integer)
(put 'project 'integer integer->scheme-number)

(define (rational->real x)
  (let ((n (car x))
        (d (cdr x)))
  (attach-tag 'integer (round (/ n d)))))

;(define (scheme-number->tower x)
;  (cond ((fixnum? x) (make-integer x))
;;        ((flonum? x) (make-real x))
;;        ((rational? x) (get-nearest-rational x)) ;; gosh では有理数がサポートされていない
;        ((flonum? x)
;         (let ((nearest-rational (get-nearest-rational x)))
;           (if (< (cdr nearest-rational) 1000)
;               (attach-tag 'rational nearest-rational)
;               (attach-tag 'real x))))
;        ((complex? x) (make-complex-from-real-imag (real-part x) (imag-part x)))
;        (else (error "unknown type -- SCHEME-NUMBER->TOWER" x))))

(define (raise-to-real x)
  (if (pair? x)
      (car (fit-args (list x (make-real 1))))
      (raise-to-real (scheme-number->tower x))
      ))

(put-coercion 'scheme-number 'real ;raise-to-real)
              (lambda (x) (raise-to-real (scheme-number->tower x))))

(define higher? (higher-definition '(complex real rational integer scheme-number)))
(define higher? (higher-definition '(complex real rational integer scheme-number)))
;(define higher? (higher-definition '(complex real rational integer)))
  
;  (print "RAISE " (type-tag x) " TO REAL")
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))

  (put 'sin 'real
       (lambda (x) (tag (sin x))))
  (put 'cos 'real
       (lambda (x) (tag (cos x))))
  (put 'sqrt 'real
       (lambda (x) (tag (sqrt x))))
  (put 'square 'real
       (lambda (x) (tag (square x))))
  (put 'atan '(real real)
       (lambda (y x) (tag (atan y x))))

  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

;(define (apply-genericq op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (drop (apply proc (map contents args)))
;;          (apply proc (map contents args))
;          (cond ((= (length args) 1)
;                 (error "No mmm" (list op type-tags))
;                 )
;                ((= (length args) 2)
;                 (let ((type1 (car type-tags))
;                       (type2 (cadr type-tags)))
;                   (if (eq? type1 type2)
;                       (error "No method for these types" (list op type-tags))
;                       (let ((args-coerced (fit-args args)))
;                         (let ((type-tags (map type-tag args-coerced)))
;                           (let ((proc (get op type-tags)))
;                             (if proc
;                                 (drop (apply proc (map contents args-coerced)))
;                                        ;                              (apply proc (map contents args-coerced))
;                                 (error "No method for these types" (list op type-tags))
;                                 )
;                             )))))
;                 )
;                (else #f))
;          ))))


(install-real-package)

(define (apply-in-real op . arg)
  (let ((argc (length arg)))
    (cond ((= argc 1)
           ((get op 'real) (contents (raise-to-real (car arg)))))
          ((= argc 2)
           ((get op '(real real)) (contents (raise-to-real (car arg)))
            (contents (raise-to-real (cadr arg)))))
          (else #f))))

(define (sin* x) (apply-in-real 'sin x))
(define (cos* x) (apply-in-real 'cos x))
(define (sqrt* x) (apply-in-real 'sqrt x))
(define (atan* y x) (apply-in-real 'atan y x))
(define (square* x) (apply-in-real 'square x))
;  ((get 'sine 'real) (contents (raise-to-real x))))
;(define (sine x) (apply-generic 'sine (raise-to-real x)))


;; UPDATER
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
    (cons (mul r (cos* a)) (mul r (sin* a))))

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
    (mul (magnitude z) (cos* (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin* (angle z))))
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
(define (render z)
  ((get 'render (type-tag z)) (contents z)))
;  (apply (apply-generic 'render z))

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

  (put 'render 'complex
       (lambda (z) (list (real-part z) "+"
                         (imag-part z) "i")))

  'updated)

(update-rectangular-package)
(update-polar-package)
(update-complex-package)

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


;;;TEST
(define z (make-complex-from-real-imag 3 4))
(define z1 (make-complex-from-real-imag (make-rational 2 3) 5))
(define z2 (make-complex-from-mag-ang 1 (make-rational 1 2)))
(define z2 (make-complex-from-mag-ang 1 (make-rational 1 2)))
(define z3 (make-complex-from-real-imag 1.5 1.5))

(define (myprint items)
  (define (iter items)
    (if (null? items)
        (newline)
        (begin
          (display (car items))
          (iter (cdr items)))))
  (iter items))
