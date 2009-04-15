(require "./lib")
(require "./2-36") ;accumulate-n

;(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define v '(1 2 3))
(define w '(4 5 6))
(define m '((1 2 3) (4 5 6) (7 8 9)))
(define n '((2 1 0) (0 2 1) (1 0 2)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; v*w = (1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32)

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))
; m*v = (123.123 456.123 789.123) = (1+4+9 4+10+18 7+16+27) = (14 32 50)
;123 1
;456 2
;789 3

(define (transpose mat)
  (accumulate-n cons nil mat))
;123    147
;456 -> 258
;789    369
;((1 2 3) (4 5 6) (7 8 9)) --> ((1 4 7) (2 5 8) (3 6 9))
;cons

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (u) (matrix-*-vector cols u)) m)))
;123   123   123.147 123.258 123.369
;456 * 456 = 456.147 456.258 456.369
;789   789   789.147 789.258 789.369
;m:((1 2 3) (4 5 6) (7 8 9)), cols:((1 4 7) (2 5 8) (3 6 9))
;(matrix-*-vector cols.m_i):
;  ((1 4 7) (2 5 8) (3 6 9))*(1 2 3) --> (30 36 42)
;147   1   147*123   1+8+21    30
;258 * 2 = 258*123 = 2+10+24 = 36
;369   3   369*123   3+12+27   42
