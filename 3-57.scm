(require "./3-5x")

;(define-macro (delay exp) `(lambda () ,@exp))
(define-macro (delay x) `(lambda () ,x))

(define fibs
  (cons-stream 0
			   (cons-stream 1
							(add-streams (stream-cdr fibs)
										fibs))))

