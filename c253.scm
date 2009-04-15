(require "./put-get")
(require "./tag")
(require "./2-86"); これまでのあらすじ

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))

  ;; 項と項リストの表現
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
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
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))



  (define (render-poly p)
    (let ((var (variable p)))
      (define (iter terms result)
        (if (null? terms)
            (cdr result)
            (let ((t (car terms)))
              (let ((vo (cond ((= (order t) 0) '())
                              ((= (order t) 1) (list var))
                              (else (list var "^" (order t))))))
                (let ((cvo (cond ((polynomial? (coeff t)) (append '(" + (") (render-poly (cdr (coeff t))) '(")") vo))
                                 ((= (coeff t) 0) '())
                                 ((= (coeff t) 1) (cons " + " (if (null? vo) '(1) vo)))
                                 ((= (coeff t) -1) (cons " - " (if (null? vo) '(1) vo)))
                                 ((< (coeff t) 0) (cons " - " (cons (- (coeff t)) vo)))
                                 (else (cons " + " (cons (coeff t) vo))))))
                  (iter (cdr terms) (append result cvo))
                  ))))
        )
      (iter (term-list p) '())
      ))

;  (define (print-poly p)
;    (myprint (render-poly p)))

  ;; Q 2.87
  (define (=zero? p) (empty-termlist? p))

  ;システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (put 'render 'polynomial render-poly)
  
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (myprint items)
  (define (iter items)
    (if (null? items)
        (newline)
        (begin
          (display (car items))
          (iter (cdr items)))))
  (iter items))

(put 'render 'scheme-number (lambda (x) x))

(define (render p)
  (myprint ((get 'render (type-tag p)) (contents p))))

(install-polynomial-package)

(define x (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
(define y (make-polynomial 'x '((100 1) (2 2) (0 1))))
(define a (make-polynomial 'x '((1 1) (0 1))))
(define b (make-polynomial 'x '((1 1) (0 -1))))
;(define z1 (make-polynomial 'x '((2 1) (1 '(+ y 1)) (0 5))))
(define z2 (make-polynomial 'x (list '(2 1) 
                                     (list 1 (make-polynomial 'y '((1 1) (0 1))))
                                     '(0 5))))

