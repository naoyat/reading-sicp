(require "./stream")

(define (stream-map proc . argstreams) ; 引数は１つ以上の stream
  (if (stream-null? (car argstreams)) ; 最初の stream が空なら<終わり>
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; 1st's
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) ; 2nd以降

;(define cnt 0)
(define (stream-map proc . argstreams) ; 引数は１つ以上の stream
  (if (stream-null? (car argstreams)) ; 最初の stream が空なら<終わり>
      the-empty-stream
      (cons-stream
	   (let1 args (map stream-car argstreams)
;		 (set! cnt (+ 1 cnt))
;		 (print cnt)
		 (apply proc args) ; 1st's
		 )
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) ; 2nd以降
;(define (stream-car stream) (car stream))
;(define (stream-cdr stream) (force (cdr stream)))

; 3-50
; 3-51
; 3-52

;;; 3.5.2
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
			   (cons-stream 1
							(add-streams (stream-cdr fibs)
										fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integer-starting-from 3))))

(define (prime? n)
  (define (iter ps)
	(cond ((> (square (stream-car ps)) n) #t)
		  ((divisible? n (stream-car ps)) #f)
		  (else (iter (stream-cdr ps)))))
  (iter primes))

; 3-53
; 3-54
; 3-55
(define (partial-sums stream)
  (cons-stream
   (stream-car stream)
   (add-streams (stream-cdr stream) (partial-sums stream)))
  )

(define ps (partial-sums integers))

;p0 = s0
;p1 = s1+s0
;p2 = s2+s1+s0


;pn = sn
;pn = p(n-1) + sn
;0 1 2 3 ...
;  0 1 2 ...
;    0 1 ...
;      0 ...
;0 01 012 0123


;(use srfi-1)
(define (first-n n stream)
;  (set! cnt 0) ;; DEBUG
  (let loop ((n n) (rest stream) (result '()))
	(if (> n 0)
		(loop (- n 1)
			  (stream-cdr rest)
			  (cons (stream-car rest) result))
		(reverse result))))

(define (first-10 stream) (first-n 10 stream))
;(stream-cdr integers)
;(first-n 10 integers)
;(first-n 10 (partial-sums integers))

;+0 0 0 0
;+  1 1 1
;+    2 2
;+      3

;Ex3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		 (let ((s1car (stream-car s1))
			   (s2car (stream-car s2)))
		   (cond ((< s1car s2car)
				  (cons-stream s1car (merge (stream-cdr s1) s2)))
				 ((> s1car s2car)
				  (cons-stream s2car (merge s1 (stream-cdr s2))))
				 (else
				  (cons-stream s1car
							   (merge (stream-cdr s1)
									  (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
								(merge (scale-stream S 3)
									   (scale-stream S 5)))))


(use srfi-1) ; using remove
(define (my-merge . streams)
  (let* ((non-null-streams (remove stream-null? streams))
		 (minimum (apply min (map stream-car non-null-streams))))
	(cons-stream minimum
				 (apply my-merge (map (lambda (stream)
										(if (= minimum (stream-car stream))
											(stream-cdr stream)
											stream))
									  non-null-streams)
						))))

(define myS (cons-stream 1
						 (my-merge (scale-stream myS 2)
								   (scale-stream myS 3)
								   (scale-stream myS 5)
								   )))
