(require "./lib")

(define (for-each func items)
  (cond ((null? items) #t)
        (else
         (func (car items))
         (for-each func (cdr items))
         )))

(define (for-each2 func items)
  (if (null? items)
      #t
      (begin 
        (func (car items))
        (for-each func (cdr items))
        )))
