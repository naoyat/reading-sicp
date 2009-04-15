(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

;a
;(cadaddr a)
;(print (cadr (caddr a)))
;b
;(print (caar b))
;c
;(cadr (cdddddr c))
;(print (cadr (cdr (cddddr c))))
;(cadadadadadadr c)
(cadr (cadr (cadr (cadr (cadr (cadr c))))))
