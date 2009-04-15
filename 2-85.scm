;(require "./2-79"); equ? ; 2-78, 2-77
(require "./2-84") ; loads 2-83, 2-82, 2-78, 2-77
;(require "./2-83")
(require "./equ")

;; 実数に近い分数を得たい。
(define (get-nearest-rational x)
  (define (fract-part r)
    (- r (round r)))
  (define (iter d)
    (if (> d 1000)
        (cons (round (* x 1000)) 1000)
        (let ((xd (* x d)))
          (if (< (abs (fract-part xd)) 0.0001)
              (cons (round xd) d)
              (iter (+ d 1))
              ))))
  (iter 1))

(define (install-project-package)
  (define (complex->real x)
    (attach-tag 'real (real-part x))) ;; (make-real (real-part x))
  (define (real->rational x)
    (attach-tag 'rational (get-nearest-rational x)))
  (define (rational->integer x)
    (let ((n (car x))
          (d (cdr x)))
      (attach-tag 'integer (round (/ n d)))))

  (put 'project 'complex complex->real)
  (put 'project 'real real->rational)
  (put 'project 'rational rational->integer)

  'done)

(define (project x)
;  (apply-generic 'project x))
    (let ((proc (get 'project (type-tag x))))
      (if proc
          (proc (contents x))
          #f)))
; raise は 2-83

(define (drop x)
  (if (pair? x)
      (let ((projected (project x)))
        (if projected
            (if (equ? (raise projected) x)
                (drop projected)
                x)
            x)
        )
      x ;through, for passing #t, etc.
      ))

;;;;
(install-project-package)
(define i2 (make-integer 3))
(define d2 (make-rational 6 3))
(define r2 (make-real 5.0))
(define c2 (make-complex-from-real-imag 1 0))

;; apply-generic, based on 2-84
;(define (apply-generic-with-drop op . args)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
;          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((args-coerced (fit-args args)))
                      (let ((type-tags (map type-tag args-coerced)))
                        (let ((proc (get op type-tags)))
                          (if proc
                              (drop (apply proc (map contents args-coerced)))
;                              (apply proc (map contents args-coerced))
                              (error "No method for these types" (list op type-tags))
                              )
                          )))))
              #f)))))


(define x (make-complex-from-real-imag 1 2))  ; 1+2i
(define y (make-complex-from-real-imag 1 -2)) ; 1-2i