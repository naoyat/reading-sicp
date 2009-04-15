(require "./52rc")

(define the-machine
  (make-machine
   '(product counter n)
   (list (list '* *)
		 (list '+ +)
		 (list '> >))
   '(controller
	 (assign product (const 1))
	 (assign counter (const 1))
	 test-counter
	 (test (op >) (reg counter) (reg n))
	 (branch (label iter-done))
	 (assign product (op *) (reg product) (reg counter))
	 (assign counter (op +) (reg counter) (const 1))
	 (goto (label test-counter))
	 iter-done)))

(set-register-contents! the-machine 'n 5)
(start the-machine)
(print (get-register-contents the-machine 'product))
