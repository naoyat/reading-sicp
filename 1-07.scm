(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
;  (print "  improve guess: " guess " >> " (average guess (/ x guess)))
  (average guess (/ x guess)))


(define (good-enough? guess x)
;  (print "  good-enough?: " (- (square guess) x))
  (print " (g*g, x) = (" (square guess) ", " x ") ... " (- (square guess) x) )
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
;  (print "(sqrt-iter " guess " " x ")")
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (print (sqrt 9))
; (print (sqrt (+ 100 37)))

; (print (sqrt 0.0001))

; |guess2 - micro-x| < 0.001 になればいいなら
; microなxに対し 誤差0.001 とは・・・
; guess = √0.001 = 0.03162277660168379331 で guess^2 = 0.001
; guess = 

;big: 浮動小数点の精度のせいで、guessが減って行かない 
;;(guess + x/guess)/2 だが
;ちがう？ 1e46でダメ。
;good-enough が... 

;(sqrt-iter 1.0e23 1.0e46)
;  good-enough?: -1.2676506002282294e30
;  Guess: 1.0e23 >> 1.0e23
;で、| 1.0e23^2

;(* 1.0e23 1.0e23) = 9.999999999999999e45
;で good-enough が -1.2676506002282294e30 だから。
;0.001 はあまりにも小さすぎる。x に応じた threshold を決めるべきだ

; 筆算でもできるんだけど


(use slib)
(require 'trace)
(trace sqrt-iter)
(trace improve)
(trace good-enough?)
;(trace new-if)

