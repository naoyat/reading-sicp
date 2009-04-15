(define (make-account balance password)
  (let ((incorrect-password-counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (say-incorrect dummy)
      (if (= incorrect-password-counter 7)
          (call-the-cops)
          "Incorrect password"))
    (define (dispatch pwd m)
      (if (eq? password pwd)
          (begin
            (set! incorrect-password-counter 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))
          (begin
            (set! incorrect-password-counter (+ incorrect-password-counter 1))
            say-incorrect)))
    dispatch))

(define (call-the-cops)
  "keisatsu ni tsu-ho- shimashita.")


;gosh> (define acc (make-account 100 'secret-password))
;acc
;gosh> ((acc 'secret-password 'withdraw) 40)
;60
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
;gosh> ((acc 'some-other-password 'deposit) 50)
;"keisatsu ni tsu-ho- shimashita."
