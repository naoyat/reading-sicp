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

  (try-it (+ 1 (random (- n 1)))))
;  (if (2 <= n-2)
;      (try-it (random-integer-between 2 (- n 2)))
      

(define (expmod* base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((a (expmod* base (/ exp 2) m))
                (r (remainder (square a) m)))
           (if (and (< 1 a)
                    (< a (- m 1))
                    (= r 1))
               0
               r)))
        (else
         (remainder (* base (expmod* base (- exp 1) m))
                    m))))

(define (expmod*- base exp m)
  (define (check-nontrivial-sqrt-1 a)
    (define (check-if-1 r)
      (if (and (< 1 a)
               (< a (- m 1))
               (= r 1))
          0
          r))
    (check-if-1 (remainder (square a) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt-1 (expmod* base (/ exp 2) m)))
;          (remainder (square (expmod* base (/ exp 2) m))
;                     m)))
;         下の (expmod* base (/ exp 2) m) = 1 になる?
; (base^(exp/2) mod m)^2 mod m
; = i^2 mod m
           ; (expmod* base (/ exp 2) m)) が 1 のケースは該当させない
           ; が、1 が返るケースとは
        (else
         (remainder (* base (expmod* base (- exp 1) m))
                    m))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

