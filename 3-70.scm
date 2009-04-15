(require "./3-5x")
;(require "./3-59")
(require "./3-66")
; p.203
;3.70
;(define weight (lambda (i j) (+ i j)))

;(define (merge-weighted s1 s2 weight)
;  (if (stream-null? s1)
;	  s2
;	  (cons-stream (stream-car s1)
;				   (interleave s2 (stream-cdr s1)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		 (let ((s1car (stream-car s1))
			   (s2car (stream-car s2)))
		   (cond ((< (weight s1car) (weight s2car))
				  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
				 ((> (weight s1car) (weight s2car))
				  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
				 (else
				  (cons-stream s1car
							   (merge-weighted (stream-cdr s1)
											   (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
	weight)
   ))


(define (weight1 p)
  (let ((i (car p)) (j (cadr p)))
	(+ i j)))
;(define ij (weighted-pairs integers integers weight))
;(define w (merge-weighted ij ij weight))
(define a (weighted-pairs integers integers weight1))

(define (first-n n stream)
  (let loop ((n n) (rest stream) (result '()))
        (if (> n 0)
                (loop (- n 1)
                          (stream-cdr rest)
                          (cons (stream-car rest) result))
                (reverse result))))

(define (weight2 p)
  (let ((i (car p)) (j (cadr p)))
	(+ (* 2 i) (* 3 j) (* 5 i j))))

(define (not235? x)
  (and (> (remainder x 2) 0)
	   (> (remainder x 3) 0)
	   (> (remainder x 5) 0)))
  
(define (filter2 p)
  (let ((i (car p)) (j (cadr p)))
	(and (not235? i) (not235? j))))

(define b (stream-filter filter2
						 (weighted-pairs integers integers weight2)))

;3.71
(define (weight3 p)
  (let ((i (car p)) (j (cadr p)))
	(+ (* i i i) (* j j j))))

(define (cont-filter weight stream last-weight)
  (let1 car-weight (weight (stream-car stream))
	(if (= last-weight car-weight)
		(cons-stream (stream-car stream)
					 (cont-filter weight (stream-cdr stream) car-weight))
		(cons-stream #f
					 (cont-filter weight (stream-cdr stream) car-weight))
		)))

(define c ;(stream-filter identity
  (cont-filter
   weight3
   (weighted-pairs integers integers weight3)
   -1));)


(define int-pairs (pairs integers integers))
(define (hoge s1 s2)
  (cons-stream (cons (stream-car s1) (stream-car s2))
			   (hoge (stream-cdr s1) (stream-cdr s2))))
(define int-pairs-with-n (hoge int-pairs integers))

(define int-pairs-with-n*
  (letrec ((hoge (lambda (s1 s2)
				   (cons-stream (cons (stream-car s1) (stream-car s2))
								(hoge (stream-cdr s1) (stream-cdr s2))))))
	(hoge int-pairs integers)
	))
