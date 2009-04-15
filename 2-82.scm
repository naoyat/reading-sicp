(require "./c252")
(require "./2-78")

(define (repeat n tag)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (cons tag result))))
  (iter n '()))

(define (coerce-all-args args target-type)
;  (print "coerce " args " to " target-type)
  (if (null? args)
      '()
      (let ((first-arg (car args)))
        (let ((t->t (if (eq? (type-tag first-arg) target-type)
                        identity
                        (get-coercion (type-tag first-arg) target-type))))
          (if t->t
              (let ((coerced-rest-args (coerce-all-args (cdr args) target-type)))
                (if coerced-rest-args
                    (cons (t->t first-arg) coerced-rest-args)
                    #f))
              #f)))
      ))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (try-to-coerce-into-every-type types-to-try)
      (if (null? types-to-try)
          #f
          (let ((proc (get op (repeat (length args) (car types-to-try)))))
            (if proc
                (let ((coerced-contents (coerce-all-args args (car types-to-try))))
                  (if coerced-contents
                      (apply proc coerced-contents)
                      (try-to-coerce-into-every-type (cdr types-to-try))))
                (try-to-coerce-into-every-type (cdr types-to-try))
                ))))
;    (print type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args)) ; そのまんまのがあれば
          (try-to-coerce-into-every-type type-tags))
      )))

(define (add . args) (apply apply-generic (cons 'add args)))

(put 'add '(scheme-number scheme-number scheme-number)
     (lambda (x y z) (+ x y z)))
(put 'add '(complex complex complex)
     (lambda (x y z) (add (add (cons 'complex x)
                               (cons 'complex y))
                          (cons 'complex z))))
