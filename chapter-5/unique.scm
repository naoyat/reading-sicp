(define (unique seq)
  (let loop ((rest seq)
			 (result '()))
	(if (null? rest)
		result
;		(reverse! result)
		(loop (cdr rest)
			  (let ((top (car rest)))
				(if (memq top result)
					result
					(cons top result)))
			  ))))

(define (symbol-sort seq)
  (sort seq
		(lambda (x y) (string<? (symbol->string x) (symbol->string y)))))

;(unique '(a b c a b c 1 1 1 1))
