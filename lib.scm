(define nil '())
; math
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (average x y) (/ (+ x y) 2))

; (define (even? n) (= (remainder n 2) 0))
; (define (odd? n) (= (remainder n 2) 1))
; (define (double n) (* n 2))
; (define (halve n) (/ n 2))
(define (even? n) (= (logand n 1) 0))
(define (odd? n) (= (logand n 1) 1))
(define (double n) (ash n 1))
(define (halve n) (ash n -1))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; system
;(define (runtime)
;  (let ((t (current-time)))
;    (+ (- (slot-ref t 'second) 1143000000)
;       (* 0.001 (/ (slot-ref t 'nanosecond) 1000000)))
;    ))
(define (runtime)
  (- (time->seconds (current-time)) 1222495600))
;  (- (time->seconds (current-time)) 1136041200))

(use srfi-27)
;(define (random n)
;  (random-integer n))
(define (random n) ;; for Q3.05 <monte-carlo>
  (if (integer? n)
      (random-integer n)
      (* (random-real) n)))

(define (random-integer-between a b)
  (if (<= a b)
      (+ a (random (+ (- b a) 1)))
      a))

(define (random-positive-integer-n-and-less n)
  (+ 1 (random n)))

;;;;;;;;;;
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
                    

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
