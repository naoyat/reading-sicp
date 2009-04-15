(define zero (lambda (f) (lambda (x) x)))

;(define (zero f)
;  (lambda (x) x))

(define (add-1 n) (lambda (f)
                    (lambda (x)
                      (f ((n f) x))) ; f( [n(f)](x) )
                    )
  )

;(define one (add-1 zero))
;: (lambda (f)
;    (lambda (x)
;      (f ((zero f) x))))
;ここで (zero f)
;: ((lambda (f) (lambda (x) x)) f)
;: (lambda (x) x)
;one
;: (lambda (f)
;    (lambda (x)
;      (f ((lambda (x) x) x))))
;: (lambda (f)
;    (lambda (x) (f x)))

;(define two (add-1 one))
;: (lambda (f)
;    (lambda (x)
;      (f ((one f) x))))
;ここで (one f)
;: ((lambda (f) (lambda (x) (f x))) f)
;: (lambda (x) (f x))
;two
;: (lambda (f)
;    (lambda (x)
;      (f ((lambda (x) (f x)) x))))
;: (lambda (f) (lambda (x) (f (f x))))

; 0 - (lambda (f) (lambda (x) x))
; 1 - (lambda (f) (lambda (x) (f x)))
; 2 - (lambda (f) (lambda (x) (f (f x))))

;(zero f)
;= [(lambda (f) (lambda (x) x)) f]
;=              (lambda (x) x)
;((zero f) x)
;= [(lambda (x) x) x]
;=              x
;(f ((zero f) x))
;= (f x)

;(one f)
;= [(lambda (f) (lambda (x) (f x))) f]
;=              (lambda (x) (f x))
;((one f) x)
;= [(lambda (x) (f x)) x]
;=              (f x)
;(f ((one f) x))
;= (f (f x))

;(two f)
;= [(lambda (f) (lambda (x) (f (f x)))) f]
;=              (lambda (x) (f (f x)))
;((two f) x)
;= [(lambda (x) (f (f x))) x]
;=              (f (f x))
;(f ((two f) x))
;= (f (f (f x)))

;(define (add-1 LAST)
;  (lambda (f) (lambda (x)
;                (f [(LAST f) x])
;                )
;         )
;  )
(define one   (add-1 zero))
(define two   (add-1 one))
(define three (add-1 two))
(define four  (add-1 three))
(define five  (add-1 four))

;(define zero (lambda (f) (lambda (x) x)))
(define one  (lambda (f) (lambda (x) (f x))))
(define two  (lambda (f) (lambda (x) (f (f x)))))

(define (check n)
  (define (inc x) (+ 1 x))
  ((n inc) 0))

(define (add n m) (lambda (f)
                    (lambda (x)
;                      (f ((n f) x)))
;                      (f (((m f) (n f)) x))
                      ((m f) ((n f) x))
                      )))

;(one f): (lambda (x) (f x))
; ((one f) x) : (f x)
;(two f): (lambda (x) (f (f x)))
; ((two f) x) : (f (f x))
;求めるのは
;(lambda (x) (f (f (f x))))
;               -------
