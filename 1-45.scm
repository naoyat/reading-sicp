(require "./1-35") ; fixed-point
(require "./1-43") ; repeated
(require "./1-25") ; fast-expt

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fixed-point! f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (quad-root x)
  (fixed-point (average-damp (lambda (y) (/ x (cube y))))
               1.0))

(define (** base exp)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (* result base))))
  (iter exp 1))

(define (nth-root-test x nth k)
  (fixed-point
   ((repeated0 average-damp k) (lambda (y) (/ x (fast-expt y (- nth 1)))))
;   ((repeated average-damp k) (lambda (y) (/ x (** y (- nth 1)))))
   1.0))

(define (nth-root x n)
  (let ((k (round (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp k) (lambda (y) (/ x (fast-expt y (- n 1)))))
                 1.0)))
