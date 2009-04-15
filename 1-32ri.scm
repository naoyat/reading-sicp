(require "./1-32")

(define (accumulate combiner null-value term a next b)
  (define (list-up a items)
    (if (> a b)
        items
        (list-up (next a) (cons (term a) items))))
  (define (iter items result)
    (if (null? items) result
        (iter (cdr items) (combiner (car items) result))))
  (iter (list-up a nil) null-value))
