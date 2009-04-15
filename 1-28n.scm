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


(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a)) ;; expmod だけを変更
;  (try-it (+ 1 (random (- n 1)))))
  (if (detect-1-modulo-n-as-nontrivial-square-root n)
      #f
      (try-it (random-integer-between 2 (- n 2)))))
; not in expmod.

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

