(require "./524rc-518")

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =)
		 (list '* *)
		 (list '- -))
   '(controller
	 (reg-trace-on n)
	 (reg-trace-on val)
	 (reg-trace-on continue)
;	 (perform (op reg-trace-on) (reg n))
;	 (perform (op reg-trace-on) (reg val))
;	 (perform (op reg-trace-on) (reg continue))
	 (perform (op initialize-stack))
	 (perform (op reset-instruction-count))
	 (perform (op trace-on))

	 (assign continue (label fact-done))
 fact-loop
	 (test (op =) (reg n) (const 1))
	 (branch (label base-case))
	 (save continue)
	 (save n)
	 (assign n (op -) (reg n) (const 1))
	 (assign continue (label after-fact))
	 (goto (label fact-loop))
 after-fact
	 (restore n)
	 (restore continue)
	 (assign val (op *) (reg n) (reg val))
	 (goto (reg continue))
 base-case
	 (assign val (const 1))
	 (goto (reg continue))
 fact-done
	 (perform (op print-stack-statistics))
	 (perform (op print-instruction-count))
	 )
   ))

(let loop ()
  (set-register-contents! fact-machine 'n (read))
  (start fact-machine)
  (print (get-register-contents fact-machine 'val))
  (loop))
