(require "./lib")

(define nil '())
(define s (list 1 2 3))

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;(define (length sequence)
;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (op x) (cons (car s) x))
;        (append rest (map op rest)))))

(define r (list nil (list 3) (list 2) (list 2 3)))