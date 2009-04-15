(require "./3-03")

(define (make-joint master-acc master-password joint-password)
  (define (dispatch passwd m)
    (if (eq? passwd joint-password)
        (master-acc master-password m)
        (lambda (x) "Incorrect password")
        ))
  dispatch)

;; test
(define peter-acc (make-account 100 'honyarara))
(define paul-acc (make-joint peter-acc 'honyarara 'rosebud))
