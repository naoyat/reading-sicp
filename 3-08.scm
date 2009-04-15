(define (make-func-1)
  (let ((last-value 1))
    (define (func x)
      (set! last-value (* last-value x))
      last-value)
    func))

(define f (make-func-1))
(print (+ (f 0) (f 1)))

(define f (make-func-1))
(print (+ (f 1) (f 0)))

;; #2
(define (make-func-2)
  (let ((last-value 0))
    (define (func x)
      (let ((ret-value last-value))
        (set! last-value x)
        ret-value))
    func))

(define f (make-func-2))
(print (+ (f 0) (f 1)))

(define f (make-func-2))
(print (+ (f 1) (f 0)))


;;#3
(define (make-func-3)
  (let ((last-value 100))
    (define (func x)
      (set! last-value (cond ((= x 0) (- 1 last-value))
                             ((= x 1) (- last-value))
                             (else last-value)))
      last-value)
    func))

(define f (make-func-3))

(print (+ (f 2) (f 0)))
(print (+ (f 3) (f 4)))
(print (+ (f 0) (f 1)))
(print (+ (f 1) (f 0)))
