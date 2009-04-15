(define (make-account balance password)
  (let ((error-counter 0))
    ; カウンタ回り
    (define (reset-error-counter)
      (set! error-counter 0)
      0)
    (define (increment-error-counter)
      (set! error-counter (+ error-counter 1))
      error-counter)

    ; 口座操作
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (say-incorrect dummy)
      (if (= (increment-error-counter) 7)
          (call-the-cops)
          "Incorrect password"))

    (define (dispatch pwd m)
      (if (eq? password pwd)
          (begin
            (reset-error-counter)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))
          say-incorrect))
    dispatch))

(define (call-the-cops)
  "This has been reported to the police.") ;警察に通報しますた

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
