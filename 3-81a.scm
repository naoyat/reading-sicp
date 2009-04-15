(require "./stream")
(require "./3-05")
(require "./3-06") ;rand-update
;(require "./3-81")

;;;; stream utilities
(define (first-n n stream)
  (let loop ((n n) (rest stream) (result '()))
        (if (> n 0)
                (loop (- n 1)
                          (stream-cdr rest)
                          (cons (stream-car rest) result))
                (reverse result))))

(define (stream-nth s n)
  (if (= n 1) (stream-car s)
	  (stream-nth (stream-cdr s) (- n 1))))

(define (list->stream l)
  (if (null? l)
	  the-empty-stream
	  (cons-stream (car l) (list->stream (cdr l)))))

(define (stream->list s . args)
  (let iter ([r s]
			 [n (if (null? args) 100 (car args))])
	(if (or (= n 0) (stream-null? r))
		'()
		(cons (stream-car r) (iter (stream-cdr r) (- n 1))))))

;; random
(random-source-randomize! default-random-source)
(define (random n)
  (* (random-real) n))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; 3.81
(define (random-number-generator request-stream initial-value)
  (define rand-stream
    (if (stream-null? request-stream)
        the-empty-stream
        (let ((req (stream-car request-stream)))
          (cons-stream
           (cond ((eq? req 'generate) (rand-update initial-value))
                 ((number? req) req)
                 (else (error "Unknown request -- RANDOM-NUMBER-GENERATOR" req)))
           (random-number-generator (stream-cdr request-stream) (stream-car rand-stream))
           ))))
  rand-stream)

(define req-stream
;  (list->stream '(1 generate generate 2 generate)))
  (cons-stream 1
               (cons-stream 'generate
                            (cons-stream 'generate
                                         (cons-stream 2
                                                      (cons-stream 'generate
                                                                   the-empty-stream))))))
(define rn (random-number-generator req-stream 0))

;(define req2-stream
;  (cons-stream 1
;               (cons-stream 'unknown
;                            (cons-stream 'generate
;                                         (cons-stream 2
;                                                      (cons-stream 'generate
;                                                                   the-empty-stream))))))
;
;(define rn2 (random-number-generator req2-stream 0))

;; 3.82 - redo 3.5
(define (monte-carlo-st experiment)
  (define (iter s n)
	(let1 new-s (if (experiment) (+ s 1) s)
	  (cons-stream (/ new-s (+ n 1)) (iter new-s (+ n 1))) ))
  (iter 0 0))

(define (estimate-integral-st P x1 x2 y1 y2)
  (define (test-func) (P (random-in-range x1 x2)
						 (random-in-range y1 y2)))
  (let1 S (* (- x2 x1) (- y2 y1))
	(stream-map (lambda (x) (* x S)) (monte-carlo-st test-func))))


(define s1 (estimate-integral-st P1 2 8 4 10))
;(define s1 (estimate-integral-st P1 0 10 0 10))
;(define s1pi (stream-map (lambda (x) (* 1.0 (/ x 9))) s1))
;(define s1pi (stream-map (lambda (x) (/ x 9.0)) s1))
(define s1pi (stream-map (cut / <> 9.0) s1))
