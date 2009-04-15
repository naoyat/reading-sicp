(require "./1-41") ;double
(require "./1-42") ;compose

(define (repeated- proc n)
  (cond ((= n 0) identity)
        ((even? n) (double (repeated proc (/ n 2))))
        (else (compose proc (repeated proc (- n 1))))))

(define (repeated proc n)
  (define (iter a b i)
    (cond ((= i 0) a)
          ((even? i)
           (iter a (double b) (/ i 2)))
          (else
           (iter (compose a b) b (- i 1)))))
  (iter identity proc n))

(define (square x) (* x x))
(define (inc x) (+ x 1))

