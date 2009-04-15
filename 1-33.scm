(require "./1-21") ; prime?
(require "./1-32r") ; accumulate

(define (filtered-accumulate combiner null-value term a next b filter-func)
  (cond ((> a b) null-value)
        ((filter-func a)
         (combiner (term a)
                   (accumulate combiner null-value term (next a) next b)))
        (else
         (accumulate combiner null-value term (next a) next b))))


(define (sum-of-squared-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-of-rp-integers n)
  (define (relatively-prime-to-n? i) (= (gcd i n) 1))
  (filter-accumulate * 1 identity 1 inc (- n 1) relatively-prime-to-n?))