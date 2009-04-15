(require "./52rc")

(define machine-54a
  (make-machine
   '(n b val continue)
   (list (list '= =)
		 (list '- -)
		 (list '* *))
   '(controller
	 (assign continue (label expt-done))
	 expt-loop
	 (test (op =) (reg n) (const 0))
	 (branch (label base-case))
	 (save continue)
	 (assign n (op -) (reg n) (const 1))
	 (assign continue (label after-expt))
	 (goto (label expt-loop))
	 after-expt
	 (restore continue)
	 (assign val (op *) (reg b) (reg val))
	 (goto (reg continue))
	 base-case
	 (assign val (const 1))
	 (goto (reg continue))
	 expt-done)))

(set-register-contents! machine-54a 'b 2)
(set-register-contents! machine-54a 'n 10)
(start machine-54a)
(print (get-register-contents machine-54a 'val))

;
(define machine-54b
  (make-machine
   '(b counter product)
   (list (list '= =)
		 (list '- -)
		 (list '* *))
   '(controller
	 ;(assign counter (reg n))
	 (assign product (const 1))
	 expt-iter
	 (test (op =) (reg counter) (const 0))
	 (branch (label expt-done))
	 (assign counter (op -) (reg counter) (const 1))
	 (assign product (op *) (reg product) (reg b))
	 (goto (label expt-iter))
	 expt-done)))

(set-register-contents! machine-54b 'b 2)
(set-register-contents! machine-54b 'counter 11)
(start machine-54b)
(print (get-register-contents machine-54b 'product))

