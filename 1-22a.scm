(require "./lib")
(require "./1-21")

(define (timed-test f)
  (define (display-result result elapsed-time)
    (if result
        (let ((msg "T"))
          (newline)
          (display "START...")
          (newline)
          (display msg)
          (newline)
          (display "DONE.")
          (newline)
          (display elapsed-time)
          )
        ))

  (define (test start-time)
    (let ((result (f)))
      (display-result result (- (runtime) start-time))
      )
    )

  (test (runtime))
  )

(define (timed-prime-test n)
  (timed-test (lambda () (prime? n))))
