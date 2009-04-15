(define (make-path . points)
  (define (iter rest result)
    (if (null? (cdr rest))
        result
        (iter (cdr rest) (cons
                          (make-segment (car rest) (cadr rest))
                          result))))
  (iter points '()))

(define (make-path-circ . points)
  (let ((start-point (car points)))
    (define (iter rest result)
      (if (null? (cdr rest))
          (cons
           (make-segment (car rest) start-point)
           result)
          (iter (cdr rest) (cons
                            (make-segment (car rest) (cadr rest))
                            result))))
    (iter points '())))

