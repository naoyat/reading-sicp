; p340-341
;
; 個別コードジェネレータへの振り分け
;
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
		 (compile-self-evaluating exp target linkage))
		((quoted? exp) (compile-quoted exp target linkage))
		((variable? exp)
		 (compile-variable exp target linkage))
		((assignment? exp)
		 (compile-assignment exp target linkage))
		((definition? exp)
		 (compile-definition exp target linkage))
		((if? exp) (compile-if exp target linkage))
		((lambda? exp) (compile-lambda exp target linkage))
		((begin? exp)
		 (compile-sequence (begin-actions exp)
						   target
						   linkage))
		((cond? exp) (compile (cond->if exp) target linkage))
		((application? exp)
		 (compile-application exp target linkage))
		(else
		 (error "Unknown expression type -- COMPILE" exp))))


; p342
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

; p343
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
		 (make-instruction-sequence '(continue) '()
									'((goto (reg continue)))))
		((eq? linkage 'next)
		 (empty-instruction-sequence))
		(else
		 (make-instruction-sequence '() '()
									`((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
			  instruction-sequence
			  (compile-linkage linkage)))

; p344
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
					(make-instruction-sequence '() (list target)
											   `((assign ,target (const ,exp))))))

(define (compile-quoted