;(require "./1-22")
(require "./lib")
(require "./1-21t")

;(prime? n)

;(use slib)
;(require 'trace)

;(trace prime?)
;(trace smallest-divisor)
;(trace find-divisor)
;(trace divides?)
;(trace remainder)
;(trace square)

(define (timed-test f n)
  (start-test f n (runtime)))

(define (start-test f n start-time)
  (f n)
  (report (- (runtime) start-time)))

(define (report elapsed-time)
  (display elapsed-time))

;
(define (timed-test2 f a b)
  (start-test2 f a b (runtime)))

(define (start-test2 f a b start-time)
  (f a b)
  (report (- (runtime) start-time)))
