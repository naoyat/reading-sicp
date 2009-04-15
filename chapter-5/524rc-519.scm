(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  #f))
;;
;;§5.2 レジスタ計算機シミュレータ
;;

;§5.2.1 計算機モデル
;(make-machine <レジスタ名リスト> <演算子リスト> <制御器>)
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
	(for-each (lambda (register-name)
				((machine 'allocate-register) register-name))
			  register-names)
	((machine 'install-operations) ops)
	((machine 'install-instruction-sequence)
	 (assemble controller-text machine))
	machine))

;レジスタ
(define (make-register name)
  (let ((contents '*unassigned*)
		(trace-mode 'off))
	(define (dispatch message)
	  (cond ((eq? message 'get) contents)
			((eq? message 'set)
			 (lambda (value)
			   (if (eq? trace-mode 'on)
				   (print (format "[~a] ~a => ~a" name contents value))
				   '*trace-off*)
			   (set! contents value)
			   ))
			((eq? message 'trace-on) (set! trace-mode 'on))
			((eq? message 'trace-off) (set! trace-mode 'off))
			(else
			 (error "Unknown request -- REGISTER" message))))
	dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;スタック
(define (make-stack)
  (let ((s '())
		(number-pushes 0)
		(max-depth 0)
		(current-depth 0))
	(define (push x)
	  (set! s (cons x s))
	  (set! number-pushes (+ 1 number-pushes))
	  (set! current-depth (+ 1 current-depth))
	  (set! max-depth (max current-depth max-depth)))
	(define (pop)
	  (if (null? s)
		  (error "Empty stack -- POP")
		  (let ((top (car s)))
			(set! s (cdr s))
			(set! current-depth (- current-depth 1))
			top)))
	(define (initialize)
	  (set! s '())
	  (set! number-pushes 0)
	  (set! max-depth 0)
	  (set! current-depth 0)
	  'done)
	(define (print-statistics)
;	  (print "STACK: print statistics")
;	  (newline)
;	  (display (list 'total-pushes  '= number-pushes
;					 'maximum-depth '= max-depth)))
	  (print (format ">>\t(total-pushes = ~d maximum-depth = ~d)" number-pushes max-depth)))
	(define (dispatch message)
	  (cond ((eq? message 'push) push)
			((eq? message 'pop) (pop))
			((eq? message 'initialize) (initialize))
			((eq? message 'print-statistics)
			 (print-statistics))
			(else (error "Unknown request -- STACK" message))))
	dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(use srfi-1)
(require "./unique")
;基本計算機
(define (make-new-machine)
  (let ((pc (make-register 'pc))
		(flag (make-register 'flag))

		(instructions-list '())
		(registers-for-entry '())
		(registers-for-save/restore '())
		(registers-and-data-sources '())

		(instruction-count 0) ; 5.15
		(trace-mode 'off) ; 5.16
		(stack (make-stack))
		(the-instruction-sequence '())
		(the-labels '()))
;		(breakpoints '()))
	(let ((the-ops
		   (list (list 'initialize-stack
					   (lambda () (stack 'initialize)))
				 (list 'print-stack-statistics
					   (lambda () (stack 'print-statistics)))
				 (list 'reset-instruction-count
					   (lambda () (set! instruction-count 0)))
				 (list 'print-instruction-count
					   (lambda () (print (format ">>\t(instruction-count ~d)" instruction-count))))
				 (list 'trace-on
					   (lambda () (set! trace-mode 'on)))
				 (list 'trace-off
					   (lambda () (set! trace-mode 'off)))
				 ))
		  (register-table
		   (list (list 'pc pc) (list 'flag flag))))
	  (define (allocate-register name)
		(if (assoc name register-table)
			(error "Multiply defined register: " name)
			(set! register-table
				  (cons (list name (make-register name))
						register-table)))
		'register-allocated)
	  (define (lookup-register name)
		(let ((val (assoc name register-table)))
		  (if val
			  (cadr val)
			  (error "Unknown register:" name))))
	  ;5.15
	  (define (reset-instruction-count)
		(set! instruction-count 0))
	  (define (increment-instruction-count)
		(set! instruction-count (+ 1 instruction-count)))

	  (define (execute)
		(let ((insts (get-contents pc)))
		  (if (null? insts)
			  'done
			  (let ((the-inst (car insts)))
				(if (eq? trace-mode 'on)
					(print 
					 (if (instruction-label the-inst)
						 (format "~a:\n" (instruction-label the-inst))
						 "")
					 (format "   ~a" (instruction-text the-inst)))
					'*trace-off*);fi
				((instruction-execution-proc the-inst))
				(increment-instruction-count)
				(if (null? (cdr insts))
					(execute) ;; nullということは直前にbreakpointが仕込まれることはない。
					(let ((next-inst (cadr insts)))
					  (if (instruction-breakpoint? next-inst)
						  (print "*break*")
						  (execute))))
				);let the-inst
			  )))

	  ; 5.12
	  (define (get-instruction-list) the-instruction-sequence)
	  (define (get-register-names) (cddr (reverse! (map car register-table))))

	  ; 5.19
	  (define (set-breakpoint label n)
		(let ((label-insts (assoc label the-labels)))
		  (let ((target-inst (list-ref label-insts n)))
			(set-instruction-breakpoint! target-inst)
			)))
	  (define (cancel-breakpoint label n)
		(let ((label-insts (assoc label the-labels)))
		  (let ((target-inst (list-ref label-insts n)))
			(unset-instruction-breakpoint! target-inst)
			)))
	  (define (cancel-all-breakpoints)
		(for-each unset-instruction-breakpoint! the-instruction-sequence))

	  (define (analyze-sequence seq) ; 5.12
		(let ((insts (filter (lambda (item) (pair? item)) seq)))
		  (set! instructions-list (symbol-sort (unique (map caar insts))))
										;entry registers
		  (let ((f (filter (lambda (item) (and (eq? 'goto (caar item))
											   (eq? 'reg (caadar item)))) insts)))
			(set! registers-for-entry (symbol-sort (unique (map cadr (map cadar f))))))
										;save/restore
		  (let ((f (filter (lambda (item) (or (eq? 'save (caar item))
											  (eq? 'restore (caar item)))) insts)))
			(set! registers-for-save/restore (symbol-sort (unique (map cadar f)))))
										;sources
;		  (set! registers-and-data-sources '())
		  (let ((reg-names (symbol-sort (get-register-names))))
			(let ((table (map (lambda (reg-name) (list reg-name)) reg-names)))
			  (let loop ((rest insts))
				(if (null? rest)
					(map (lambda (item) 
						   (set! registers-and-data-sources
								 (cons item ;(cons (car item)
											 ;(reverse (cdr item)))
									   registers-and-data-sources)))
						 table) ; ending
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
					)))))
		)

	  (define (show-analytics) ; 5.12
		(print "* instructions-list:\n\t" instructions-list)
		(print "* registers-for-entry:\n\t" registers-for-entry)
		(print "* registers-for-save/restore:\n\t" registers-for-save/restore)
		(print "* registers-and-data-sources:")
		(for-each (lambda (item)
					(print "\t" (car item) ":")
					(for-each (lambda (item) (print "\t\t" item)) (cdr item))
					)
				  registers-and-data-sources)
		)

	  (define (dispatch message)
		(cond ((eq? message 'start)
			   (set-contents! pc the-instruction-sequence)
			   (execute))
			  ((eq? message 'proceed) ; 5.19
			   (print "*proceed*")
			   (execute))

			  ((eq? message 'install-instruction-sequence)
			   (lambda (seq)
				 (set! the-instruction-sequence seq)
				 (analyze-sequence seq)))

			  ((eq? message 'allocate-register) allocate-register)
			  ((eq? message 'get-register) lookup-register)
			  ((eq? message 'install-operations)
			   (lambda (ops) (set! the-ops (append the-ops ops))))
			  ((eq? message 'stack) stack)
			  ((eq? message 'operations) the-ops)

			  ((eq? message 'reset-instruction-count) (reset-instruction-count)) ; 5.15
			  ((eq? message 'increment-instruction-count) (increment-instruction-count)) ; 5.15

			  ((eq? message 'show-analytics) (show-analytics)) ; 5.12

			  ((eq? message 'set-labels)
			   (lambda (labels) (set! the-labels labels))) ; 5.19
			  ((eq? message 'set-breakpoint)
			   (lambda (label n) (set-breakpoint label n))) ; 5.19
			  ((eq? message 'cancel-breakpoint)
			   (lambda (label n) (cancel-breakpoint label n))) ; 5.19
			  ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints)) ; 5.19
			  
			  (else (error "Unknown request -- MACHINE" message))))
	  dispatch)))

;(start <機械モデル>)
(define (start machine)
  (machine 'start))

(define (proceed machine) ; 5.19
  (machine 'proceed))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

;(get-register-contents <機械モデル> <レジスタ名>)
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

;(set-register-contents! <機械モデル> <レジスタ名> <値>)
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;[p.311]
(define (assemble controller-text machine)
  (extract-labels controller-text
				  (lambda (insts labels)
					(update-insts! insts labels machine)
					insts)
				  #f))

(define (extract-labels text receive last-inst) ;; (receive insts labels)
  (if (null? text)
	  (receive '() '())
	  (let ((next-inst (car text)))
		(extract-labels (cdr text)
						(lambda (insts labels)
						  (if (symbol? next-inst) ; if label
							  (receive insts
									   (cons (make-label-entry next-inst
															   insts)
											 labels))
							  (receive (cons (make-instruction next-inst
															   last-inst)
											 insts)
									   labels)))
						(if (symbol? next-inst) next-inst #f)))
	  ))

;[p.312]
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
		(flag (get-register machine 'flag))
		(stack (machine 'stack))
		(ops (machine 'operations)))
	((machine 'set-labels) labels)
	(for-each
	 (lambda (inst)
	   (set-instruction-execution-proc!
		inst
		(make-execution-procedure
		 (instruction-text inst) labels machine
		 pc flag stack ops)))
	 insts)))

(define (make-instruction text label)
  (list text '() #f label))				;  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (instruction-breakpoint? inst) ; optional
  (caddr inst))

(define (instruction-label inst) ; optional
  (cadddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))

(define (set-instruction-breakpoint! inst)
  (set-car! (cddr inst) #t))

(define (unset-instruction-breakpoint! inst)
  (set-car! (cddr inst) #f))

(define (set-instruction-label! inst label)
  (set-car! (cdddr inst) label))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
	(if val
		(cdr val)
		(error "Undefined label -- ASSEMBLE" label-name))))

;§5.2.3 命令の実行手続きの生成
(define (make-execution-procedure inst labels machine
								  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
		 (make-assign inst machine labels ops pc))
		((eq? (car inst) 'test)
		 (make-test inst machine labels ops flag pc))
		((eq? (car inst) 'branch)
		 (make-branch inst machine labels flag pc))
		((eq? (car inst) 'goto)
		 (make-goto inst machine labels pc))
		((eq? (car inst) 'save)
		 (make-save inst machine stack pc))
		((eq? (car inst) 'restore)
		 (make-restore inst machine stack pc))
		((eq? (car inst) 'perform)
		 (make-perform inst machine labels ops pc))
		((eq? (car inst) 'reg-trace-on)
		 (make-register-trace-on inst machine pc))
		((eq? (car inst) 'reg-trace-off)
		 (make-register-trace-off inst machine pc))
		(else (error "Unknown instruction type -- ASSEMBLE"
					 inst))))

; assign命令
(define (make-assign inst machine labels operations pc)
  (let ((target
		 (get-register machine (assign-reg-name inst)))
		(value-exp (assign-value-exp inst)))
	(let ((value-proc
		   (if (operation-exp? value-exp)
			   (make-operation-exp
				value-exp machine labels operations)
			   (make-primitive-exp
				(car value-exp) machine labels))))
	  (lambda ()
		(set-contents! target (value-proc))
		(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;
(define (make-register-trace-on inst machine pc)
  (let ((register (get-register machine (register-trace-reg-name inst))))
	(lambda ()
	  (register 'trace-on)
	  (advance-pc pc))))

(define (make-register-trace-off inst machine pc)
  (let ((register (get-register machine (register-trace-reg-name inst))))
	(lambda ()
	  (register 'trace-off)
	  (advance-pc pc))))

(define (register-trace-reg-name inst)
  (cadr inst))

; test, branch, goto
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
	(if (operation-exp? condition)
		(let ((condition-proc
			   (make-operation-exp
				condition machine labels operations)))
		  (lambda ()
			(set-contents! flag (condition-proc))
			(advance-pc pc)))
		(error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
	(if (label-exp? dest)
		(let ((insts
			   (lookup-label labels (label-exp-label dest))))
		  (lambda ()
			(if (get-contents flag)
				(set-contents! pc insts)
				(advance-pc pc))))
		(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;[p.315]
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
	(cond ((label-exp? dest)
		   (let ((insts
				  (lookup-label labels
								(label-exp-label dest))))
			 (lambda () (set-contents! pc insts))))
		  ((register-exp? dest)
		   (let ((reg
				  (get-register machine
								(register-exp-reg dest))))
			 (lambda ()
			   (set-contents! pc (get-contents reg)))))
		  (else (error "Bad GOTO instruction -- ASSEMBLE"
					   inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

; その他の命令
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (push stack (get-contents reg))
	  (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (set-contents! reg (pop stack))
	  (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
	(if (operation-exp? action)
		(let ((action-proc
			   (make-operation-exp
				action machine labels operations)))
		  (lambda ()
			(action-proc)
			(advance-pc pc)))
		(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

; 部分式の実行手続き
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
		 (let ((c (constant-exp-value exp)))
		   (lambda () c)))
		((label-exp? exp)
		 (let ((insts
				(lookup-label labels
							  (label-exp-label exp))))
		   (lambda () insts)))
		((register-exp? exp)
		 (let ((r (get-register machine
								(register-exp-reg exp))))
		   (lambda () (get-contents r))))
		(else
		 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
		(aprocs
		 (map (lambda (e)
				(make-primitive-exp e machine labels))
			  (operation-exp-operands exp))))
	(lambda ()
	  (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

;[p.317]
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
	(if val
		(cadr val)
		(error "Unknown operation -- ASSEMBLE" symbol))))

