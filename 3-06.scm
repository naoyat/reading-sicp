(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296)) ;線形合同法だけどね

(define (make-rand)
  (let ((last-random-number 1))
    (define (generate)
      (set! last-random-number (rand-update last-random-number))
      last-random-number)
    (define (reset new-value)
      (set! last-random-number new-value)
      new-value)
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate)) ;; 
            ((eq? m 'reset) reset)
            ((eq? m 'last) last-random-number)
            (else (error "Unknown request -- MAKE-RAND"
                         m))))
    dispatch))

(define rand (make-rand))
