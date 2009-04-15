(require "./3-5x")

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

;¡ø3.5.3
(define (sqrt-improve guess x)
  (average guess (/ x guess)))


(define (sqrt-stream x)
  (define guesses
	(cons-stream 1.0
				 (stream-map (lambda (guess)
							   (sqrt-improve guess x))
							 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
		[s1 (stream-ref s 1)]
		[s2 (stream-ref s 2)])
	(cons-stream (- s2 (/ (square (- s2 s1))
						  (+ s0 (* -2 s1) s2)))
				 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
			   (make-tableau transform
							 (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
			  (make-tableau transform s)))

;3.63
(define (sqrt-stream x)
  (cons-stream 1.0
			   (stream-map (lambda (guess)
							 (sqrt-improve guess x))
						   (sqrt-stream x))))

;3.64
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;3.65
;(define ln2-stream
;  (cons-stream 1
;			   (

;(define (pairs s t)
(define (stream-append s1 s2)
  (if (stream-null? s1)
	  s2
	  (cons-stream (stream-car s1)
				   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
	  s2
	  (cons-stream (stream-car s1)
				   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(pairs (stream-cdr s) (stream-cdr t)))))

;;;;;;
(define pii (pairs integers integers))
(define (1-100? p) (equal? '(1 100) p))
(define (99-100? p) (equal? '(99 100) p))
(define (100-100? p) (equal? '(100 100) p))

(define (find proc s)
  (let loop ((s s) (n 1))
	(if (proc (stream-car s))
		n
		(loop (stream-cdr s) (+ n 1))
		)))
  
;(print (find 1-100? (pairs integers integers)))
(define (find-pair pair)
  (find (lambda (p) (equal? pair p))
		(pairs integers integers)))

;3.67
(define (pairs67 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
	(interleave
	 (stream-map (lambda (x) (list (stream-car s) x))
				 (stream-cdr t))
	 (stream-map (lambda (x) (list x (stream-car t)))
				 (stream-cdr s)))
	(pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs67c s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(pairs67c (stream-cdr s) t))))

;3.68
(define (pairs68 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) ; s^ . t
			   t)
   (pairs68 (stream-cdr s) (stream-cdr t))))

(define (pairs68r s t)
  (interleave
   (pairs68r (stream-cdr s) (stream-cdr t))
   (stream-map (lambda (x) (list (stream-car s) x)) ; s^ . t
			   t)
   ))

;3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
	(stream-map (lambda (p) (cons (stream-car s) p))
				(pairs (stream-cdr t)
					   (stream-cdr u)))
	(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))) ))

(define (pythagoras? triple)
  (let ((i (car triple))
		(j (cadr triple))
		(k (caddr triple)))
	(= (+ (* i i) (* j j)) (* k k)) ))

(define pythagoras (stream-filter pythagoras? (triples integers integers integers)))

