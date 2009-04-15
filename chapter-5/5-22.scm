(require "./53rc"); 524のままで行ける

(define append-machine
  (make-machine
   '(x y val a continue)
   (list (list 'car car)
		 (list 'cdr cdr)
		 (list 'cons cons)
		 (list 'null? null?))
'(controller
  (reg-trace-on x)
  (reg-trace-on y)
  (reg-trace-on val)
  (reg-trace-on a)

  (assign continue (label done))
 append ;; (append x y)
	(test (op null?) (reg x))
	(branch (label if-null))
; if-not-null
	(assign a (op car) (reg x))
	(assign x (op cdr) (reg x))
	(save a)
	(save continue) ; [continue a ...]
	(assign continue (label after-append))
	(goto (label append))
 after-append
   ;   a = (car x)
   ; val = (append (cdr x) y)
	(restore continue) ; [x ...]
	(restore a)
	(assign val (op cons) (reg a) (reg val))
	(goto (reg continue))
 if-null
	(assign val (reg y))
	(goto (reg continue))
 done)))

(set-register-contents! append-machine 'x '(a b c))
(set-register-contents! append-machine 'y '(x y z))
(start append-machine)
(print (get-register-contents append-machine 'val))


(define append!-machine
  (make-machine
   '(x y d continue)
   (list (list 'car car)
		 (list 'cdr cdr)
		 (list 'cons cons)
		 (list 'set-cdr! set-cdr!)
		 (list 'null? null?))
'(controller
  (reg-trace-on x)
  (reg-trace-on y)
;  (reg-trace-on val)
  (reg-trace-on d)

;	(assign continue (label done))
	(save x)
 last-pair
	(assign d (op cdr) (reg x))      ; d = (cdr x)
	(test (op null?) (reg d))        ; if (null? (cdr x)) goto last-pair-done
	(branch (label last-pair-done))
	(assign x (reg d))               ; x = (cdr x)
	(goto (label last-pair))         ; goto last-pair
 last-pair-done
    ; x = (last-pair x)
	(perform (op set-cdr!) (reg x) (reg y)) ; (set-cdr! (last-pair x) y)
	(restore x)
 done)))

(set-register-contents! append!-machine 'x '(a b c))
(set-register-contents! append!-machine 'y '(x y z))
(start append!-machine)
(print (get-register-contents append!-machine 'x))

