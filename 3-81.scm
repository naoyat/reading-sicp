(require "./stream")
(require "./3-06") ;rand-update

(define (random-number-generator request-stream initial-value)
  (define rand-stream
    (if (stream-null? request-stream)
        the-empty-stream
        (let ((req (stream-car request-stream)))
          (cons-stream
           (cond ((eq? req 'generate) (rand-update initial-value))
                 ((number? req) req)
                 (else (error "Unknown request -- RANDOM-NUMBER-GENERATOR" req)))
           (random-number-generator (stream-cdr request-stream) (stream-car rand-stream))
           ))))
  rand-stream)

(define req-stream
  (cons-stream 1
               (cons-stream 'generate
                            (cons-stream 'generate
                                         (cons-stream 2
                                                      (cons-stream 'generate
                                                                   the-empty-stream))))))
(define rn (random-number-generator req-stream 0))

(define req2-stream
  (cons-stream 1
               (cons-stream 'unknown
                            (cons-stream 'generate
                                         (cons-stream 2
                                                      (cons-stream 'generate
                                                                   the-empty-stream))))))

(define rn2 (random-number-generator req2-stream 0))
