(require "./1-42")

(define (repeated0 proc n)
  (define (iter i result)
    (if (= i 0) result
        (iter (- i 1) (compose result proc))
        ))
  (iter n identity))

(define (repeated proc n)
  (define (iter i result)
    (if (= i 1) result
        (iter (- i 1) (compose result proc))
        ))
  (iter n proc))

(define (square x) (* x x))
(define (inc x) (+ x 1))