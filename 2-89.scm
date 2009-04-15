(require "./c253")
(require "./2-80a") ;; =zero?

; dense version

;; based on install-polynomial-package in 2-87
(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable (simplify-poly term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))

  ;; 項と項リストの表現
;  (define (adjoin-term term term-list)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (coeff term) term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (polynomial? p)
    (if (and (pair? p) (eq? (car p) 'polynomial)) #t #f))
  ;;
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (reverse (add-terms (reverse (term-list p1))
                                       (reverse (term-list p2)))))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (add-terms L1 L2) ; 0 1 2 3 ... n
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else (cons (add (car L1) (car L2))
                      (add-terms (cdr L1) (cdr L2))))))

  (define (sub-poly p1 p2)
    (add-poly p1 (minus-poly p2)))

  (define (minus-poly p)
    (define (reverse-signs terms)
      (if (null? terms)
          '()
          (cons (- (car terms))
                
;          (let ((coeff (car terms)))
;            (adjoin-term
;            (make-term (order term) (- (coeff term)))
                (reverse-signs (cdr terms)))
          ))
    (cons (variable p) (reverse-signs (term-list p)))
    )

  (define (simplify-poly term-list)
      (cond ((null? term-list) '())
            ((= (car term-list) 0) (simplify-poly (cdr term-list)))
            (else term-list)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (reverse (mul-terms (reverse (term-list p1))
                                       (reverse (term-list p2)))))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2) ;; 0 1 2 3 ... n
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (cons 0 (mul-terms (rest-terms L1) L2)))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (map (lambda (x) (* x t1)) L)))

  (define (render-poly p)
    (let ((var (variable p)))
      (define (iter terms result)
        (if (null? terms)
            (cdr result)
            (let ((coeff (car terms))
                  (order (length (cdr terms))))
              (let ((vo (cond ((= order 0) '())
                              ((= order 1) (list var))
                              (else (list var "^" order)))))
                (let ((cvo (cond ((polynomial? coeff) (append '(" + (") (render-poly (cdr coeff)) '(")") vo))
                                 ((= coeff 0) '())
                                 ((= coeff 1) (cons " + " (if (null? vo) '(1) vo)))
                                 ((= coeff -1) (cons " - " (if (null? vo) '(1) vo)))
                                 ((< coeff 0) (cons " - " (cons (- coeff) vo)))
                                 (else (cons " + " (cons coeff vo))))))
                  (iter (cdr terms) (append result cvo))
                  ))))
        )
      (if (empty-termlist? (term-list p))
          '(0)
          (iter (term-list p) '()))
      ))

;  (define (print-poly p)
;    (myprint (render-poly p)))

  (define (=zero-polynomial? p) (empty-termlist? p)) ;;2-87

  ;システムの他の部分とのインターフェース
;  (define (tag p) (attach-tag 'polynomial p))
  (define (tag p)
    (let ((terms (term-list p)))
      (cond ((empty-termlist? terms) 0)
            ((= (length terms) 1) (car terms))
            (else  (attach-tag 'polynomial p)))
      ))


  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (put 'render 'polynomial render-poly)
  (put '=zero? 'polynomial =zero-polynomial?) ;;2-87
  
  'done)

(install-polynomial-package)

(define a (make-polynomial 'x '(1 2 3 -2 -5)))
(define b (make-polynomial 'x '(1
                                0 0 0 0 0 0 0 0 0 0 ;90
                                0 0 0 0 0 0 0 0 0 0 ;80
                                0 0 0 0 0 0 0 0 0 0 ;70
                                0 0 0 0 0 0 0 0 0 0 ;60
                                0 0 0 0 0 0 0 0 0 0 ;50
                                0 0 0 0 0 0 0 0 0 0 ;40
                                0 0 0 0 0 0 0 0 0 0 ;30
                                0 0 0 0 0 0 0 0 0 0 ;20
                                0 0 0 0 0 0 0 0 0 0 ;10
                                0 0 0 0 0 0 0 2 0 1)))
(define c (make-polynomial 'x '(1 1)))
(define d (make-polynomial 'x '(1 -1)))
;(define z1 (make-polynomial 'x '((2 1) (1 '(+ y 1)) (0 5))))
(define e (make-polynomial 'x (list 1
                                    (make-polynomial 'y '(1 1))
                                    5)))


;gosh> (mul a b)
;(polynomial x 1 0 -1)
;gosh> (mul b b)
;(polynomial x 1 -2 1)
;gosh> (add (mul b b) (mul a a))
;(polynomial x 2 0 2)
;gosh> (sub (mul b b) (mul a a))
;(polynomial x -4 0)
