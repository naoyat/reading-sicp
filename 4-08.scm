(define (fib n)
  (let ((fib-iter '*unassigned*))
	(set! fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
	(fib-iter 1 0 n)))

;(fib 10)
