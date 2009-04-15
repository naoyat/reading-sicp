;(define (runtime2)
;  (let ((t (current-time)))
;    (+ (- (slot-ref t 'second) 1.143e9)
;       (* 0.001 (/ (slot-ref t 'nanosecond) 1000000)))
;    ))

;(define (runtime)
;  (let ((t (current-time)))
;    (+ (- (slot-ref t 'second) 1136041200)
;       (* 0.001 (/ (slot-ref t 'nanosecond) 1000000)))
;    ))

(define (runtime)
  (- (time->seconds (current-time)) 1136041200))
