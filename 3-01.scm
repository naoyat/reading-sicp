(define (make-accumulator initial-value)
  (define (accumulator increment)
    (begin
      (set! initial-value (+ initial-value increment))
      initial-value))
  accumulator)


;gosh> (define A (make-accumulator 5))
;A
;gosh> (A 10)
;15
;gosh> (A 10)
;25
;gosh> (A 100)
;125
