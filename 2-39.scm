(require "./lib")
(require "./2-38") ; fold-left

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define x '(a b c d e f g))
