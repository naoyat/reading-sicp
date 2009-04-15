(require "./52rc-513B")
; アセンブル時アロケート

(define fib-machine
  (make-machine
;   '(n val continue)
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

(set-register-contents! fib-machine 'n (read))
(start fib-machine)
(print (get-register-contents fib-machine 'val))
