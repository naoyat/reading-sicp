(require "./53rc"); 524のままで行ける

; a)
; (define (count-leaves tree)
;   (cond ((null? tree) 0)
; 		((not (pair? tree)) 1)
; 		(else (+ (count-leaves (car tree))
; 				 (count-leaves (cdr tree))))))

(define count-leaves-machine-a
  (make-machine
   '(tree val tmp continue)
   (list (list '+ +)
		 (list 'car car)
		 (list 'cdr cdr)
;		 (list 'not not)
		 (list 'null? null?)
		 (list 'pair? pair?))
'(controller
	 (reg-trace-on tree)
	 (reg-trace-on val)
	 (reg-trace-on tmp)
;	 (reg-trace-on continue)
	 (perform (op trace-on))

	 (assign continue (label end))
 count-leaves
	 (test (op null?) (reg tree))
	 (branch (label if-null)) ; (branch (reg continue)) ができると楽ちん
; if-not-null
;	 (assign tmp (op pair?) (reg tree))
;	 (test (op not) (reg tmp))
;	 (branch (label if-not-pair))
	 (test (op pair?) (reg tree))
	 (branch (label if-pair))
; if-not-pair
	 (assign val (const 1))
	 (goto (reg continue))
 if-pair
	 (save continue)  ;[continue ...]
	 (assign continue (label after-car))
	 (save tree)      ;[元tree continue ...]
	 (assign tree (op car) (reg tree))
	 (goto (label count-leaves))
 after-car
	 (restore tree)   ;[continue ...]
	 (assign continue (label after-cdr))
	 (save val)
	 (save tree)      ;[元tree {(car tree)のカウント} continue ...]
	 (assign tree (op cdr) (reg tree))
	 (goto (label count-leaves))
 after-cdr
	 (restore tree)   ;[{(car tree)のカウント} continue ...]
;	 (assign tmp (reg val)) ; tmp = val = (cdr tree)のカウント
;	 (restore val)    ;[continue ...]
	 (restore tmp)    ;tmpに元valを受けることで１命令節約
	 (assign val (op +) (reg tmp) (reg val))  ; val = 旧val + 新val = (car tree)のカウント + (cdr tree)のカウント
	 (restore continue) ;[...]
	 (goto (reg continue))
 if-null
	 (assign val (const 0))
	 (goto (reg continue))
 end)))

(set-register-contents! count-leaves-machine-a 'tree '(1 2 (3 4 (5) 6) 7 (8 (9)) 10 11))
(start count-leaves-machine-a)
(print (get-register-contents count-leaves-machine-a 'val))

; b)
; (define (count-leaves tree)
;   (define (count-iter tree n)
; 	(cond ((null? tree) n)
; 		  ((not (pair? tree)) (+ n 1))
; 		  (else (count-iter (cdr tree)
; 							(count-iter (car tree) n)))))
;   (count-iter tree 0))

(define count-leaves-machine-b
  (make-machine
   '(tree n continue)
   (list (list '+ +)
		 (list 'car car)
		 (list 'cdr cdr)
;		 (list 'not not)
		 (list 'null? null?)
		 (list 'pair? pair?))
'(controller
	 (reg-trace-on tree)
	 (reg-trace-on n)
;	 (reg-trace-on continue)
	 (perform (op trace-on))
	 (perform (op initialize-stack))

	 (assign continue (label end))
 count-leaves
	 (assign n (const 0)) ;; (count-iter tree 0)

 count-iter
	 (test (op null?) (reg tree))
	 (branch (label if-null))
; if-not-null
	 (test (op pair?) (reg tree))
	 (branch (label if-pair))
; if-not-pair
	 (assign n (op +) (reg n) (const 1))
	 (goto (reg continue))
 if-pair
	 (save continue)    ; [continue ...]
	 (assign continue (label after-car))
	 (save tree)        ; [tree continue ...]
	 (assign tree (op car) (reg tree))
	 (goto (label count-iter))
 after-car
	 (restore tree)     ; [continue ... ]
;;	 (assign continue (label after-cdr))
	 (restore continue)
	 (assign tree (op cdr) (reg tree))
	 (goto (label count-iter))  ;; after-cdrではなく、元のcontinueに帰ればいい。
;; after-cdr
;;	 (restore continue) ; [...]
;;	 (goto (reg continue))
 if-null
	 (goto (reg continue))
 end
	 (perform (op print-stack-statistics))
	 )))

(set-register-contents! count-leaves-machine-b 'tree '(1 2 (3 4 (5) 6) 7 (8 (9)) 10 11))
(start count-leaves-machine-b)
(print (get-register-contents count-leaves-machine-b 'n))
