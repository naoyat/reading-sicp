(require "./lib")

(define (same-parity . w)
  (let ((first-even? (even? (car w))))
    (define (same-parity? item)
      (let ((this-even? (even? item)))
        (or (and this-even? first-even?)
            (and (not this-even?) (not first-even?)))))
    (define (iter items result)
      (cond ((null? items) result)
            ((same-parity? (car items))
             (cons (car items) (iter (cdr items) result)))
            (else
             (iter (cdr items) result))))
    (iter w nil)))

