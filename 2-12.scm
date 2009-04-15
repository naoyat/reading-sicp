(require "./2-7")

(define (make-center-percent c p)
;  (let ((w (* 0.01 p c)))
  (let ((w (/ (* c p) 100)))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2)) ; Æ±¤¸

(define (percent i)
  (let ((w (/ (- (upper-bound i)
                 (lower-bound i))
              2)))
    (* (/ w (center i)) 100)))

;sample
(define r1 (make-center-percent 6.8 10))
(define r2 (make-center-percent 4.7 5))

