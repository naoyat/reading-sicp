(require "./put-get")

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;
(define (install-sum-package)
  (define (addend operands)
    (car operands))

  (define (augend operands)
    (let ((rest (cdr operands)))
      (if (null? (cdr rest))
          (car rest)
          (cons '+ rest))))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  )

(define (install-product-package)
  (define (multiplier operands)
    (car operands))

  (define (multiplicand operands)
    (let ((rest (cdr operands)))
      (if (null? (cdr rest))
          (car rest)
          (cons '* rest))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (deriv-product exp var)
    ((get 'make '+)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
)

(define (install-exponentiation-package)
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  (define (make-exponentiation b exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) b)
          (else (list '** b exp))))

  (define (deriv-exponentiation exp var)
    ((get 'make '*)
     ((get 'make '*) (exponent exp)
                   (make-exponentiation (base exp) (- (exponent exp) 1)))
     (deriv (base exp) var)))

  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation)
)

(install-sum-package)
(install-product-package)
(install-exponentiation-package)
