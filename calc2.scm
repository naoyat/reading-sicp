;(define (% a b) (remainder a b))
;(define (\ a b) (quotient a b))
(define (% a b) (remainder (+ a b) b))
(define (\ a b) (- (quotient (+ a b) b) 1))

(define (caddddr x) (car (cddddr x)))
(define (cadddddr x) (cadr (cddddr x)))

(define origin '(1 1 1 0 1 1))
(define zero '(0 0 0 0 0 0))

(define (s50! n)
  (let ((t (\ n 50)))
    (map (lambda (x) (* x t)) '(0 1 1 0 1 1))))

(define (s25! n)
  (let* ((t (\ n 50))
         (<t> (/ (* t (+ t 1)) 2))
         (h (\ (+ n 25) 50))
         (<h> (/ (* h (+ h 1)) 2))
         )
    (map +
         (map (lambda (x) (* x <t>)) '(0 0 1 0 1 1)) ; t(t+1)/2
         (map (lambda (x) (* x <h>)) '(0 0 0 1 1 1)) ; h(h+1)/2
         ))
  )
  
(define (s10! n)
  (let* ((i (\ n 10))
         (q (\ i 5))
         (r (% i 5))
         (<i> (+
               (/ (* 5 q (+ q 1) (+ q 2)) 6); 5q(q+1)(q+2)/6
               (/ (* r (+ q 1) (+ q 2)) 2); r(q+1)(q+2)/2
               ))
         )
    
                                        ;   
    (map (lambda (x) (* x <i>)) '(0 0 0 0 1 1)) ; t(t+1)/2
    )
  )

(define (s10h! n)
  (let* ((i (\ (- n 25) 10))
;         (q (\ i 5))
;         (r (% i 5))
         (q (\ i 5))
         (r (% i 5))
         (<i> (+
               (/ (* 5 q (+ q 1) (+ q 2)) 6); 5q(q+1)(q+2)/6
               (/ (* r (+ q 1) (+ q 2)) 2); (q+1)(q+2)/2
               ))
         )

    (map (lambda (x) (* x <i>)) '(0 0 0 0 1 1)) ; t(t+1)/2
    )
  )

(define (s5! n)
;  (let* ((t (\ n 10))
;         (k (\ n 5)))
;         (h (\ (+ n 25) 10))
;         ))
    (define (iter i sum)
      (let* ((m (- i 1))
             (t (\ m 50)) (tr (% m 50))
             (h (\ (+ m 25) 50))
             )
        (if (> i n)
            sum
            (iter (+ i 5) (+ sum
;                             1 ;origin
                             ;t ;50
;                             (/ (* t (+ t 1)) 2) ; t(t+1)/2 <25>
                             ;(/ (* h (+ h 1)) 2) ; h(h+1)/2 <25>

                                        ;10
;                             (let* ((j (\ m 10))
;                                    (q (\ j 5))
;                                    (r (% j 5)))
;                               (+
;                                (/ (* 5 q (+ q 1) (+ q 2)) 6); 5q(q+1)(q+2)/6
;                                (/ (* r (+ q 1) (+ q 2)) 2); (q+1)(q+2)/2
;                                ))
                                        ;10h
                             (let* ((j (\ (- m 25) 10))
                                    (q (\ j 5))
                                    (r (% j 5)))
                               (+
                                (/ (* 5 q (+ q 1) (+ q 2)) 6); 5q(q+1)(q+2)/6
                                (/ (* r (+ q 1) (+ q 2)) 2); (q+1)(q+2)/2
                                ))
                             );+
                  );iter
;             (iter (+ i 5) (+ sum (caddddr (map + origin (s25! i-1) (s10! i-1) (s10h! i-1)))))
;             )
            );fi
        ))

    (define (sum-func q r scale func sigma-func)
      (+
       (* scale (sigma-func (- q 1)))
       (* r (func q))
       ))

;;;
    (define (s50 q r)
      (sum-func q r 10
                (lambda (t) t)
                (lambda (t) (/ (* t (+ t 1)) 2)) ; t(t+1)/2
                ))
    
    (define (s25 q r)
      (sum-func q r 10
                (lambda (t) (/ (* t (+ t 1)) 2)) ; t(t+1)/2
                (lambda (t) (/ (* t (+ t 1) (+ t t 1)) 6)) ; t(t+1)(t+2)/6
                ))

    (define (s10 q r)
;      (let ((r0 (\ r 2)))
; r = [n/5] % 10
        (sum-func q r 2
                                        ; r0 = [n/10] % 5
                  (lambda (t) (+ (/ (* 5 t (+ t 1) (+ t 2)) 6)
                                 (/ (* r (+ t 1) (+ t 2)) 2)
                                 )) ; 5t(t+1)(t+2)/6 + r(t+1)(t+2)/2
                  (lambda (t) (+ (/ (* 5 t (+ t 1) (+ t 2) (+ t 3)) 24)
                                 (/ (* r (+ (* t t t) (* 6 t t) (* 11 t))) 6) ;; r in <10>
                                 )) ; 5t(t+1)(t+2)(t+3)/24 + r(t^3+6t^2+11t)/6
                  )
        )
    
;                                (/ (* 5 q (+ q 1) (+ q 2)) 6); 5q(q+1)(q+2)/6
 ;                               (/ (* r (+ q 1) (+ q 2)) 2); r(q+1)(q+2)/2 


    (list 0 0 0 0 0
          (let* ((k (\ n 5))  ;1,2,3,4,5...
                 (k$ (\ (+ n 25) 5)) ;6,7,8,9,10,...
                 (t (\ n 50)) ;0,0,0,0,0,...
                 (h (\ (+ n 25) 50))
                 (r (% k 10))
                                        ; (t$ (\ (+ n 25) 50)) ;(r$ (% k$ 10))
                 )
            (+
             ;1
             k
             ;50
             (s50 t r)
             ;25
             (s25 t r)
             (s25 h (% k$ 10))
             ;10
;             (let* ((i (% n 10))
;                    (iq (\ i 5))
;                    (ir (% i 5)))
;             (s10 t (% k 10) (% (\ n 10) 5))
             (s10 (\ n 10) (% k 2))

             (iter 5 0)
                                        ;           ))
             );+
            );let
      );list
    )
;  50由来
;         t
;  25由来
;         (<t> (/ (* t (+ t 1)) 2))
;         (<h> (/ (* h (+ h 1)) 2))
;  10
;  10h

;  '(0 0 0 0 0 0)


;;;
(define (50@ n) (car origin))

(define (25@ n)
  (list
   (cadr (map +
              origin
              (s50! n)
              ))
   (+ 1 (\ n 50)); [n-1/50]
   ))

(define (10@ n)
  (list
   (caddr (map +
               origin
               (s50! n)
               (s25! n)
               ))
   (/ (* (+ (\ n 50) 1) (+ (\ n 50) 2)) 2); ([(n-1)/50] + 1)([(n-1)/50] + 2)/2
;   (/ (* (\ (+ n 49) 50) (+ (\ (+ n 49) 50) 1)) 2); ([(n-1)/50]+1)([(n-1)/50]+2)/2
   ))
(define (10h@ n)
  (list
   (cadddr (map +
                origin
                (s50! n)
                (s25! n)
                ))
   (/ (* (\ (+ n 24) 50) (+ (\ (+ n 24) 50) 1)) 2); [(n-1)+25/50]([[(n-1)+25/50]+1)/2
   ))
(define (5@ n)
  (caddddr (map +
                origin
                (s50! n)
                (s25! n)
                (s10! n)
                (s10h! n)
                )))
(define (1@ n)
  (cadddddr (map +
                 origin
                 (s50! n)
                 (s25! n)
                 (s10! n)
                 (s10h! n)
                 (s5! n)
                 )))

(define (@ n)
  (list
   (map +
        origin
        (s50! n)
        (s25! n)
        (s10! n)
        (s10h! n)
        (s5! n)
        )
;   (list (50@ n) (25@ n) (10@ n) (10h@ n) (5@ n) (1@ n))
   ))

