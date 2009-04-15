;ordered

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(define evens '(2 4 6 8 10 12 14 16 18 20))
(define odds '(1 3 5 7 9 11 13 15 17 19))
(define primes '(2 3 5 7 11 13 17 19))
(define squares '(1 4 9 16))

;2.61
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((c (car set)))
        (cond ((= x c) set) ; 同じものがあった。set を返す
              ((< x c) (cons x set))
              (else (cons c (adjoin-set x (cdr set))))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set) ; 同じものがあった。set を返す
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

