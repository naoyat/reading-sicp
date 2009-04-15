(define (make-monitored f)
  (let ((counter 0))
    (define (mf arg) ;monitored-func
      (set! counter (+ counter 1))
      (f arg))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) counter)
            (else (mf m))))
    dispatch))

;gosh> (define s (make-monitored sqrt))
;s
;gosh> s
;#<closure (make-monitored dispatch)>
;gosh> (s 100)
;10.0
;gosh> (s 'how-many-calls?)
;1
;gosh> (s 100)
;10.0
;gosh> (s 'how-many-calls?)
;2

