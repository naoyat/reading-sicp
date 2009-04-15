(require "./3-5x")

;(define (expand num den radix)
;  (cons-stream
;   (quotient (* num radix) den)
;   (expand (remainder (* num radix) den) den radix)))

;a
(define (integrate-series stream)
  (stream-map / stream integers))
;  (mul-series stream (stream-map / integers)))
;  (define (div-stream num-stream den-stream)
;	(cons-stream (/ (stream-car num-stream)
;					(stream-car den-stream))
;				 (div-stream (stream-cdr num-stream) (stream-cdr den-stream)) ))
;  (div-stream stream integers))

;b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define (sigma stream till)
  (let loop ((rest stream)
			 (sum 0)
			 (cnt till))
	(if (= 0 cnt)
		(* 1.0 sum)
		(loop (stream-cdr rest) (+ sum (stream-car rest)) (- cnt 1)) )))

(define cosine-series ; 1
;  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series ; 0
  (cons-stream 0 (integrate-series cosine-series)) )

; 3-60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
			   (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
							(mul-series (stream-cdr s1) s2))))


(define s (add-streams (mul-series cosine-series cosine-series)
					   (mul-series sine-series sine-series)))

; 3-61
(define (invert-unit-series s)
; Sr: (stream-cdr s)
  (cons-stream 1
			   (scale-stream
				(mul-series (stream-cdr s) (invert-unit-series s))
				-1)
			   ))

;;;

; 3-62
(define (div-series nom-series denom-series)
  (if (= 0 (stream-car denom-series))
	  'error
	  (mul-series nom-series
				  (invert-unit-series denom-series)) ))

(define tangent-series (div-series sine-series cosine-series))

;;;;

(define (apply-power-series series x till)
  (+ (stream-car series)
	 (if (= 0 till)
		 0
		 (* x (apply-power-series (stream-cdr series) x (- till 1))))
	 ))


(define till 32)

(define (exp x) (* 1.0 (apply-power-series exp-series x till)))
(define (sin x) (* 1.0 (apply-power-series sine-series x till)))
(define (cos x) (* 1.0 (apply-power-series cosine-series x till)))
(define (tan x) (* 1.0 (apply-power-series tangent-series x till)))

(define (deg->rad deg) (* 3.141592653589793238462643383279 (/ deg 180)))
(define (sinD deg) (sin (deg->rad deg)))
(define (cosD deg) (cos (deg->rad deg)))
(define (tanD deg) (tan (deg->rad deg)))

