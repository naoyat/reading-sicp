(define-macro (delay x) `(memo-proc (lambda () ,x))) ; memoize
;(define-macro (delay x) `(lambda () ,x)) ; no memoize
(define (force x) (x))

(define-macro (cons-stream a b) `(cons ,a (delay ,b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;    ((cons-stream h t)
;      (cons h (delay t)))))
;(define-syntax cons-stream
;  (syntax-rules ()
;    ((cons-stream h t)
;      (cons h (delay t)))))

;;
;; p188
;;
; the-empty-stream, stream-null? - as in MIT Scheme's definition
(define the-empty-stream '())

(define stream-null? null?)

; stream-ref
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;
;; p189
;;
; stream-map
;;(define (stream-map proc s)
;;  (if (stream-null? s)
;;      the-empty-stream
;;      (cons-stream (proc (stream-car s))
;;                   (stream-map proc (stream-cdr s)))))
;; Ex.3.50
(define (stream-map proc . argstreams) ; 引数は１つ以上の stream
  (if (stream-null? (car argstreams)) ; 最初の stream が空なら<終わり>
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; 1st's
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) ; 2nd以降
; 出力ストリームの nth = 入力各ストリームの nth を proc で演算した結果


; stream-for-each
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; display-stream, display-line
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; cons-stream, stream-car, stream-cdr
;(define-macro (cons-stream a b) `(cons ,a (delay ,b)))
;(define (stream-car stream) (car stream))
;(define (stream-cdr stream) (force (cdr stream)))

;;
;; p190
;;
; stream-enumerate-interval
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

; stream-filter
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;
;; p191
;;
; delay, force
;;(define-macro (delay x) `(lambda () ,x))
;(define-macro (delay x) `(memo-proc (lambda () ,x)))

;;(define (force x) (apply x '()))
;(define (force x) (x))

; memo-proc
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;;
;; サンプルデータ (section 3.5.2 より)
;;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;(define (fibgen a b)
;  (cons-stream a (fibgen b (+ a b))))
;(define (fibgen a b)
;  (cons-stream a 
;               (begin (newline) (print "> " a " + " b " = " (+ a b))
;                      (fibgen b (+ a b)))))

;(define fibs (fibgen 0 1))

;; p.194
;(define (add-streams s1 s2)
;  (stream-map + s1 s2))