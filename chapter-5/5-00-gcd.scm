(require "./52rc")

;(define (rem 

(define the-machine
  (make-machine
   '(t a b)
   (list (list '= =)
		 (list 'rem remainder))
   '(controller
	 test-b
	 (test (op =) (reg b) (const 0))
	 (branch (label gcd-done))
	 (assign t (op rem) (reg a) (reg b))
	 (assign a (reg b))
	 (assign b (reg t))
	 (goto (label test-b))
	 gcd-done)
   ))

(set-register-contents! the-machine 'a 56)
(set-register-contents! the-machine 'b 24)
(start the-machine)
(print (get-register-contents the-machine 'a))
