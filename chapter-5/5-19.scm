(require "./524rc-519")

(define gcd-machine
  (make-machine
   '(t a b)
   (list (list '= =)
		 (list 'rem remainder))
'(controller
	 (reg-trace-on t)
	 (reg-trace-on a)
	 (reg-trace-on b)
;	 (perform (op initialize-stack))
	 (perform (op reset-instruction-count))
	 (perform (op trace-on))

 test-b
	 (test (op =) (reg b) (const 0))
	 (branch (label gcd-done))
	 (assign t (op rem) (reg a) (reg b))
	 (assign a (reg b))
	 (assign b (reg t))
	 (goto (label test-b))
 gcd-done)
   ))

(gcd-machine 'show-analytics)

(set-register-contents! gcd-machine 'a 56)
(set-register-contents! gcd-machine 'b 24)

(set-breakpoint gcd-machine 'test-b 4)
(start gcd-machine)

(print (format "{t:~a a:~a b:~a}"
			   (get-register-contents gcd-machine 't)
			   (get-register-contents gcd-machine 'a)
			   (get-register-contents gcd-machine 'b)
			   ))

(proceed gcd-machine)

(print (format "{t:~a a:~a b:~a}"
			   (get-register-contents gcd-machine 't)
			   (get-register-contents gcd-machine 'a)
			   (get-register-contents gcd-machine 'b)
			   ))

;(let loop ()
;  (set-register-contents! gcd-machine 'n (read))
;  (start gcd-machine)
;  (print (get-register-contents gcd-machine 'val))
;  (loop))
