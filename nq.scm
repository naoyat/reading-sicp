(define (amb-n n)
  (if (= n 0)
	  (amb)
	  (amb n (amb-n (- n 1)))))

(define (n-queen n)
  (let loop ((k n) (decided '()))
	(if (= k 0) decided
		(let1 xy (cons k (amb-n n))
		  (require (not-collide? xy decided))
		  (loop (- k 1) (cons xy decided))
		  ))))

(define (not-collide? xy xys)
  (if (null? xys) #t
	  (and (not-collide-1? xy (car xys))
		   (not-collide? xy (cdr xys)))))

(define (not-collide-1? xy0 xy1)
  (let ((x0 (car xy0)) (y0 (cdr xy0))
		(x1 (car xy1)) (y1 (cdr xy1)))
	(require ((if (null? xys) #t
	  (and (not-collide-1? xy (car xys))
		   (not-collide? xy (cdr xys)))))
