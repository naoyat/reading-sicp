(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? password pwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
;        "Incorrect password"))
        (lambda (x) "Incorrect password")))
  dispatch)


;gosh> (define acc (make-account 100 'secret-password))
;acc
;gosh> ((acc 'secret-password 'withdraw) 40)
;60
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"