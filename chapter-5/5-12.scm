(require "./52rc-512")

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <)
		 (list '+ +)
		 (list '- -))
'(controller
	 (assign continue (label fib-done))
 fib-loop
	 (test (op <) (reg n) (const 2))
	 (branch (label immediate-answer))
	 ;; Fib(n-1)を計算するよう設定
	 (save continue)
	 (assign continue (label afterfib-n-1))
	 (save n)
	 (assign n (op -) (reg n) (const 1))
	 (goto (label fib-loop))
 afterfib-n-1
	 (restore n)
	 (restore continue)
	 ;; Fib(n-2)を計算するよう設定
	 (assign n (op -) (reg n) (const 2))
	 (save continue)
	 (assign continue (label afterfib-n-2))
	 (save val)
	 (goto (label fib-loop))
 afterfib-n-2
	 (assign n (reg val)) ;;-   n = 新val
	 (restore val)        ;;- val = 旧val
;	 (restore n)          ;;+   n = 旧val ; val = 新val
	 (restore continue)
	 (assign val (op +) (reg val) (reg n))
	 (goto (reg continue))
 immediate-answer
	 (assign val (reg n))
	 (goto (reg continue))
 fib-done)
   ))

;
; 5.12
;
(use srfi-1); filter
(require "./unique")
; instructions
(let ((inst-list (fib-machine 'instructions)))
  (let ((f (filter (lambda (item) (pair? item)) inst-list)))
	(print (symbol-sort (unique (map caar f))))
	))

;entry registers
(let ((inst-list (fib-machine 'instructions)))
  (let ((f (filter (lambda (item) (and (pair? item)
									   (eq? 'goto (caar item)) ;car に (goto (reg ...
									   (eq? 'reg (caadar item))
									   ))
				   inst-list)))
	(print (symbol-sort (unique
						 (map cadr (map cadar f)))))
	))

;save/restore
(let ((inst-list (fib-machine 'instructions)))
  (let ((f (filter (lambda (item) (and (pair? item)
									   (or (eq? 'save (caar item))
										   (eq? 'restore (caar item)))))
				   inst-list)))
	(print (symbol-sort (unique
						 (map cadar f))))
	))

;sources
(let ((inst-list (fib-machine 'instructions))
	  (reg-names (symbol-sort (fib-machine 'register-names))))
  (let ((table (map (lambda (reg-name) (list reg-name)) reg-names)))
	(let loop ((rest inst-list))
	  (if (null? rest)
		  (map (lambda (item) 
				 (print (car item) ":")
				 (for-each (lambda (x) (print " - " x)) (reverse (cdr item)))
				 ) table) ; ending
		  (let ((inst (car rest)))
			(if (and (pair? inst)
					 (eq? 'assign (caar inst)))
				(let ((var (cadar inst))
					  (source (cddar inst)))
				  (let ((stock (assoc var table)))
					(set-cdr! stock (cons source (cdr stock)))
					))
				'*label*)
			(loop (cdr rest)))
			))
	))

;(print "label registers:"
;	   (fib-machine 'label-registers))
;(print "stacked registers:"
;	   (fib-machine 'stacked-registers))
;(print "assignment sources:"
;	   (fib-machine 'assignment-sources))

;(set-register-contents! fib-machine 'n (read))
;(start fib-machine)
;(print (get-register-contents fib-machine 'val))
