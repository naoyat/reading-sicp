(require "./lib") ; expmod
(require "./1-21") ; prime?

(define (test-carmichael n)
  (define (iter a n)
    (if (= a n) 
        #t
        (if (= (expmod a n n) a)
            (iter (+ a 1) n)
            #f)))
  (iter 1 n))

(define (carmichael? n)
  (if (prime? n)
      #f
      (if (test-carmichael n) #t #f)))

(define (find-carmichael until)
  (define (show-carmichael n)
    (display n)
    (newline))
  (define (iter n)
    (cond ((= n until) (newline))
          ((carmichael? n)
           (show-carmichael n)
           (iter (+ n 1)))
          (else
           (iter (+ n 1)))))
  (iter 1))

(define (find-carmichael* start end)
  (define (show-carmichael n)
    (display n)
    (newline))
  (define (iter n)
    (cond ((>= n end) (newline))
          ((carmichael? n)
           (show-carmichael n)
           (iter (+ n 2)))
          (else
           (iter (+ n 2)))))
  (iter start))

(define (find-carmichael# until)
  (define (show-carmichael n)
    (display n)
    (newline))
  (define (iter n)
    (cond ((= n until) (newline))
          ((prime? n) (iter (+ n 1)))
          (else
           (if (test-carmichael n) (show-carmichael n))
           (iter (+ n 1)))))
  (iter 1)) ; 2でもいいかもしれないけど

; (find-carmichael 10000)