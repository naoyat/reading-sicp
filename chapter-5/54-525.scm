; 524からの分岐
(require "./524rc-519"); ほぼ524のまま
(require "./eval2")

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

;5.23
(define (cond? exp) (tagged-list? exp 'cond))
(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp) ; Ex4.6より
  (let ((bindings (let-bindings exp))
		(body (let-body exp)))
	(let ((vars (map car bindings))
		  (vals (map cadr bindings)))
	  (cons
	   (append (list 'lambda vars)
			   body)
	   vals)
	  )))
; var-exps : cadr
;  body : 
; (let ( (<var1> <exp1>)
;         ...
;         (<varn> <expn>) )
;        <body>)

;5.25
(define (actual-value exp env)
  (force-it (eval exp env)))

;(define (force-it obj)
;  (if (thunk? obj)
;	  (actual-value (thunk-exp obj) (thunk-env obj))
;	  obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
		 (let ((result (actual-value
						(thunk-exp obj)
						(thunk-env obj))))
		   (set-car! obj 'evaluated-thunk)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		((evaluated-thunk? obj)
		 (thunk-value obj))
		(else obj)))

(define (list-of-arg-values exps env) ; 5.25
  (map (lambda (exp) (actual-value exp env)) exps))

(define (list-of-delayed-args exps env) ; 5.25
  (map (lambda (exp) (delay-it exp env)) exps))

;;
;;
;;
;;
(define eceval-operations
  (list
		(list 'car car)
		(list 'cdr cdr)
		(list 'null? null?)
		(list 'eq? eq?)

		(list 'force-it force-it) ; 5.25
		(list 'list-of-arg-values list-of-arg-values) ; 5.25
		(list 'list-of-delayed-args list-of-delayed-args) ; 5.25

		(list 'self-evaluating? self-evaluating?)
		(list 'variable? variable?)
		(list 'quoted? quoted?)
		(list 'assignment? assignment?)
		(list 'definition? definition?)
		(list 'if? if?)
		(list 'cond? cond?)  ; 5.23
		(list 'cond->if cond->if)  ; 5.23
		(list 'let? let?)    ; 5.23
		(list 'let->combination let->combination)    ; 5.23
		(list 'lambda? lambda?)
		(list 'begin? begin?)
		(list 'application? application?)
		(list 'lookup-variable-value lookup-variable-value)
		(list 'text-of-quotation text-of-quotation)
		(list 'lambda-parameters lambda-parameters)
		(list 'lambda-body lambda-body)
		(list 'make-procedure make-procedure)
		(list 'operands operands)
		(list 'operator operator)
		(list 'empty-arglist empty-arglist)
		(list 'no-operands? no-operands?)
		(list 'first-operand first-operand)
		(list 'last-operand? last-operand?)
		(list 'adjoin-arg adjoin-arg)
		(list 'rest-operands rest-operands)
		(list 'primitive-procedure? primitive-procedure?)
		(list 'compound-procedure? compound-procedure?)
		(list 'apply-primitive-procedure apply-primitive-procedure)
		(list 'procedure-parameters procedure-parameters)
		(list 'procedure-environment procedure-environment)
		(list 'extend-environment extend-environment)
		(list 'procedure-body procedure-body)
		(list 'begin-actions begin-actions)
		(list 'first-exp first-exp)
		(list 'last-exp? last-exp?)
		(list 'rest-exps rest-exps)
		(list 'if-predicate if-predicate)
		(list 'true? true?)
		(list 'if-alternative if-alternative)
		(list 'if-consequent if-consequent)
		(list 'cond-clauses cdr)
;		(list 'cond-else-clause? cond-else-clause?)
		(list 'cond-first-clause car)
		(list 'cond-rest-clauses cdr)
		(list 'cond-predicate car)
		(list 'cond-actions cdr)
		(list 'assignment-variable assignment-variable)
		(list 'assignment-value assignment-value)
		(list 'set-variable-value! set-variable-value!)
		(list 'definition-variable definition-variable)
		(list 'definition-value definition-value)
		(list 'define-variable! define-variable!)
;		(list 'initialize-stack initialize-stack)
		(list 'prompt-for-input prompt-for-input)
		(list 'read read)
		(list 'get-global-environment get-global-environment)
		(list 'announce-output announce-output)
		(list 'user-print user-print)
		))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
;	 (perform (op trace-on))

;;
;; 5.4.4 評価の実行
;;
 read-eval-print-loop
	(perform (op initialize-stack))
	(perform (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
 print-result
    (perform (op print-stack-statistics)) ;; p.337
	(perform (op announce-output) (const ";;; EC-Eval value:"))
	(assign val (op force-it) (reg val)) ;;; 5.25 actual-value = force-it . eval
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

 unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

 unknown-procedure-type
	(restore continue)
	(assign val (const unknown-procedure-type-error))
	(goto (label signal-error))

 signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

;;
;; 5.4.1 積極制御評価器の中核
;;
 eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op cond?) (reg exp))
	(branch (label ev-cond))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op let?) (reg exp))
	(branch (label ev-let))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))
	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))

;
; 単純式の評価
;
 ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
 ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
 ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
 ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure)
			(reg unev) (reg exp) (reg env))
	(goto (reg continue))

;
; 手続き作用の評価
;

; 旧apply:
;(apply (eval (operator exp) env)
;;	   (list-of-values (operands exp) env))
;	   (map (lambda (operand) (eval operand env)) (operands exp)))
; 新apply:
;(apply (actual-value (operator exp) env)
;> (apply (force-it (eval (operator exp) env))
;	   (operands exp)
;	   env)

 ev-application
	(save continue)
	(save env)
;	(assign unev (op operands) (reg exp))
	(assign argl (op operands) (reg exp))
;	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))
 ev-appl-did-operator
;	(restore unev)
	(restore env)
;	(assign argl (op empty-arglist))
	(assign proc (op force-it) (reg val)) ; 正規+
;	(assign argl (reg unev)) ; ママ
	(perform (op user-print) (reg argl))
;	(assign proc (reg val))               ; 以下-
;	(test (op no-operands?) (reg unev))
;	(branch (label apply-dispatch))
;	(save proc)
; ev-appl-operand-loop
;	(save argl)
;	(assign exp (op first-operand) (reg unev))
;	(test (op last-operand?) (reg unev))
;	(branch (label ev-appl-last-arg))
;	(save env)
;	(save unev)
;	(assign continue (label ev-appl-accumulate-arg))
;	(goto (label eval-dispatch))
; ev-appl-accumulate-arg
;	(restore unev)
;	(restore env)
;	(restore argl)
;	(assign argl (op adjoin-arg) (reg val) (reg argl))
;	(assign unev (op rest-operands) (reg unev))
;	(goto (label ev-appl-operand-loop))
; ev-appl-last-arg
;	(assign continue (label ev-appl-accum-last-arg))
;	(goto (label eval-dispatch))
; ev-appl-accum-last-arg
;	(restore argl)
;	(assign argl (op adjoin-arg) (reg val) (reg argl))
;	(restore proc)
	(goto (label apply-dispatch))

;
; 手続き作用
;
 apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))

 primitive-apply
;#
	(save unev)
	(assign unev (reg argl))
	(assign argl (op empty-arglist))
	(test (op nullol?) (reg unev))
	(branch (label prim-empty))

prim-argl-loop
	(save argl)
	(assign exp (op car) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label prim-last-arg))
	(save env)
	(save unev)
	(assign continue (label prim-accumulate-arg))
	(goto (label eval-dispatch))
 prim-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign val (op force-it) (reg val)) ;#
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label prim-argl-loop))
 prim-last-arg
	(assign continue (label prim-accum-last-arg))
	(goto (label eval-dispatch))
 prim-accum-last-arg
	(restore argl)
	(assign val (op force-it) (reg val)) ;#
	(assign argl (op adjoin-arg) (reg val) (reg argl))

 prim-empty
	(restore unev)
;#
;	(assign argl (op list-of-arg-values) (reg argl) (reg env)) ;####
 ; argl --> (map (lambda (arg) (force-it (eval arg env))) argl) ;####
	(assign val (op apply-primitive-procedure)
			(reg proc)
			(reg argl))
	(restore continue)
	(goto (reg continue))

 compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign argl (op list-of-delayed-args) (reg argl) (reg env)) ;####
 ; argl --> (map (lambda (arg) (delay-it arg env)) argl) ;####
	(assign env (op extend-environment)
			(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

;;
;; 5.4.2 並びの評価と末尾再帰
;;
 ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))

 ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
 ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
 ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))

;
; 末尾再帰
;
; ev-sequence
;	(assign exp (op no-more-exps?) (reg unev))
;	(branch (label ev-sequence-end))
;	(assign exp (op first-exp) (reg unev))
;	(save unev)
;	(save env)
;	(assign continue (label ev-sequence-continue))
;	(goto (label eval-dispatch))
; ev-sequence-continue
;	(restore env)
;	(restore unev)
;	(assign unev (op rest-exps) (reg unev))
;	(goto (label ev-sequence))
; ev-sequence-end
;	(restore continue)
;	(goto (reg continue))

;;
;; 5.4.3 条件式、代入および定義
;;
 ev-if
	(save exp)
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch))
 ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(assign val (op force-it) (reg val)) ;;; 5.25 actual-value = force-it . eval
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
 ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
 ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))

 ev-cond
;	(assign exp (op cond->if) (reg exp))
;	(goto (label ev-if))
	(assign unev (op cond-clauses) (reg exp))
 ev-cond-rec
	(test (op null?) (reg unev))
	(branch (label ev-cond-undef))    ; if (null? unev) then 'undef

	(assign exp (op cond-first-clause) (reg unev))
	(save exp) ;[exp]
	(assign exp (op cond-predicate) (reg exp))   ; exp に pred が入る
;	(perform (op user-print) (reg exp));;;
	(test (op eq?) (reg exp) (const else))
;	(test (op cond-else-clause?) (reg exp))
	(branch (label ev-cond-true))          ; if (pred == 'else) then goto /true/
	(save unev)
	(save env)
	(save continue) ;[continue env exp]
	(assign continue (label ev-cond-pred))
	(goto (label eval-dispatch))
 ev-cond-pred
	(restore continue) ; [env exp]
	(restore env)
	(restore unev)
	(test (op true?) (reg val))
	(branch (label ev-cond-true))

; 次の項目へ
	(restore exp) ;[]
	(assign unev (op cond-rest-clauses) (reg unev))
	(goto (label ev-cond-rec))

 ev-cond-true
	(restore exp) ;[]
	(assign unev (op cond-actions) (reg exp))
	(save continue) ;; ev-sequence に行く際に必要なpush
	(goto (label ev-sequence))

 ev-cond-undef
	(assign val (const undef))
	(goto (reg continue))
;done


 ev-let
	(assign exp (op let->combination) (reg exp))
	(goto (label eval-dispatch))

;
; 代入と定義
;
 ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev)
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch))
 ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

 ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev)
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch))
 ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

 end)))

;;;;;;;
;(define the-global-environment (setup-environment))
;(define (get-global-environment) the-global-environment)
(start eceval)
