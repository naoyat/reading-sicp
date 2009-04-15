(require "./lib") ;random
(use srfi-27)
(define (random n)
  (if (fixnum? n) ; integer だと 1.0 でも true だった
      (random-integer n)
      (* (random-real) n)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test-func)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (monte-carlo trials test-func)
     (* (- x2 x1) (- y2 y1)))
  )


;(estimate-integral P 0 10 0 10 10)
(define pi 3.141592653589793238462643383279)

(define (P1 x y)
  (<= (+ (square (- x 5))
         (square (- y 7)))
      (square 3))
  )
;(estimate-integral P 2.0 8.0 4.0 10.0 100000)
(define (P2 x y)
  (and (<= 0 x) (< x 1)
       (<= 0 y) (< y 1)
       (<= y (square x)))) ; y <= x^2 ;; [1/3x^3](0..1) = 1/3

(define (P3 x y)
  (and (<= 0 y) (<= y (sin x))
       (<= 0 x) (<= x pi))) ; y <= sin x;; [-cos x](0..pi) = 2

