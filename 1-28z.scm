(require "./lib")
;(require "./1-21")
(require "./1-24")

; (one of) Fermatの小定理
; n { prime number
; 0 < a < n
; a^(n-1) === 1 (mod n)

; random
(define (detect-1-modulo-n-as-nontrivial-square-root n)
  (define (iter i)
    (cond ((= i 1) #f)
          ((= (remainder (square i) n) 1) #t)
          (else (iter (- i 1)))))
  (iter (- n 2)))


(define (small-fermat-test n)
  (not (detect-1-modulo-n-as-nontrivial-square-root n)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((small-fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

