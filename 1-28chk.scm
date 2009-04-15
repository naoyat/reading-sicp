(require "./lib")
(require "./1-21") ;; (prime? n)
;(require "./1-28")
(require "./1-28")


(define (test start end times)
  (search-for-primes start end times))

;(define (rep-test test-fn times)
 ; (cond ((= times 0) #t)
  ;      (test-fn (rep-test test-fn (- times 1)))
   ;     (else #f)))

(define (miller-rabin-test-rep n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin-test-rep n (- times 1)))
        (else #f)))

(define (search-for-primes start end times)
  (define (iter n)
    (let ((p (prime? n))
          (mr (miller-rabin-test-rep n times)))
;          (mr (rep-test (lambda () (miller-rabin-test n)) times)))
;      (display p)
;      (display ", ")
;      (display mr);
;      (newline)

      (if (or (and mr p)
              (and (not mr) (not p)))
          1
;          (begin
;            (display ".")
;            (newline))
          (begin
            (display n)
            (newline)));fi
      );let
    (if (< n end)
        (iter (+ n 1)))
    );def
  (iter start))
    
