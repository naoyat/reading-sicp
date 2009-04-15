(require "./52rc")

(define (square x) (* x x))
(define (abs x) (if (< x 0) (- x) x))
(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (if (< (abs (- (square guess) x)) 0.001) #t #f))

(define (improve guess x)
  (average guess (/ x guess)))

(define sqrt-machine
  (make-machine
   '(guess x)
   (list (list 'good-enough? good-enough?)
		 (list 'improve improve))
   '(controller
	 (assign guess (const 1.0))
	 test-guess
	 (test (op good-enough?) (reg guess) (reg x))
	 (branch (label sqrt-done))
	 (assign guess (op improve) (reg guess) (reg x))
	 (goto (label test-guess))
	 sqrt-done)
   ))

(set-register-contents! sqrt-machine 'x 2)
(start sqrt-machine)
(print (get-register-contents sqrt-machine 'guess))

;
(define sqrt-machine-b
  (make-machine
   '(guess x g2 d a t)
   (list (list 'square square)
		 (list 'abs abs)
		 (list 'average average)
		 (list '/ /)
		 (list '- -)
		 (list '< <))
   '(controller
	 (assign guess (const 1.0))
	 test-guess
	 (assign g2 (op square) (reg guess))
	 (assign d (op -) (reg g2) (reg x))
	 (assign a (op abs) (reg d)) ; a = |guess^2 - x|
	 (test (op <) (reg a) (const 0.001))
	 (branch (label sqrt-done))
	 (assign t (op /) (reg x) (reg guess))
	 (assign guess (op average) (reg guess) (reg t))
	 (goto (label test-guess))
	 sqrt-done)
   ))

(set-register-contents! sqrt-machine-b 'x 2)
(start sqrt-machine-b)
(print (get-register-contents sqrt-machine-b 'guess))
