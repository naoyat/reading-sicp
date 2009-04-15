(require "./lib")
;(require "./1-21")
(require "./1-24")

; (one of) Fermatの小定理
; n { prime number
; 0 < a < n
; a^(n-1) === 1 (mod n)

; random
;(define (fermat-test n) #f)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod* a (- n 1) n) 1)) ;; expmod だけを変更
;  (try-it (+ 1 (random (- n 1)))))
  (try-it (random-integer-between 2 (- n 2))))

(define (detect-1-modulo-n-as-nontrivial-square-root n)
  (define (iter i)
    (cond ((= i 1) #f)
          ((= (remainder (square i) n) 1) #t)
          (else (iter (- i 1)))))
  (iter (- n 2)))

(define (expmod* base exp m)
  (cond ((= exp 0) 1)
;        ((and (< 1 base)
;              (< base (- m 1))
;              (= (remainder (square base) m) 1)) 0) ;; nontrivial square root of 1 modulo n
        ((detect-1-modulo-n-as-nontrivial-square-root m) 0)
        ((even? exp)
         (remainder (square (expmod* base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod* base (- exp 1) m))
                    m))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

