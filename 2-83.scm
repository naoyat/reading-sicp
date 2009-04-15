;(require "./2-82")
(require "./c252")

(define (make-integer n)
  (attach-tag 'integer n))

(define (make-real r)
  (attach-tag 'real r))
;(define (make-real-from-numer-denom n d)
;  (attach-tag 'real (/ n d)))

(define (integer->rational x)
  (make-rational x 1))

(define (rational->real x)
  (let ((n (car x))
        (d (cdr x)))
    (make-real (/ n d))))
;  (/ (numer x) (denom x)))

(define (real->complex x)
  (make-complex-from-real-imag x 0))

(put 'raise 'integer integer->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real real->complex)

(define (raise x)
  ;(apply-generic 'raise x))
    (let ((proc (get 'raise (type-tag x))))
      (if proc
          (proc (contents x))
          x)))
;          #f)))

(define i (make-integer 3))
(define d (make-rational 2 3))
(define r (make-real 3.14))
(define c (make-complex-from-real-imag 1 2))

