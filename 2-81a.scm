(require "./c252")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)

;;a
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (expt x y)))
;     (lambda (x y) (tag (expt x y))))

(exp 3 3) ; 27
(exp 2 10) ; 1024

(define z (make-complex-from-real-imag 1 2))
;(exp z z) ; loop
;(exp z 2) ; loop
;(exp 2 z) ; loop

;; 'exp '(complex *) (* complex) の場合
; scm scm (expt)
; scm cpx (ない!) --> 有(scm->cpx) ×(cpx->scm) ; cpx,cpx
; cpx scm (ない!) --> ×(cpx->scm) 有(scm->cpx) ; cpx,cpx
; cpx cpx (ない!) --> ？(cpx->cpx) ？(cpx->cpx)
; (b)２引数が同じ型なら、どうせなにも変わらないのでそのままでいい
;（すでに試した組み合わせをさらに２回再帰して試す。再帰先でも同様なので無限に終わらない）
; (c)２引数が同じ型なら coercion を試みない版（←同じ組み合わせを試させないために！）

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
;    (print type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((a1 (car args))
                          (a2 (cadr args)))
                      (let ((t1->t2 (get-coercion type1 type2))
                            (t2->t1 (get-coercion type2 type1)))
                        (cond (t1->t2
                               (apply-generic op (t1->t2 a1) a2))
                              (t2->t1
                               (apply-generic op a1 (t2->t1 a2)))
                              (else
                               (error "No method for these types"
                                      (list op type-tags))))))
                    ))
              (error "No method for these types"
                     (list op type-tags)))))))
