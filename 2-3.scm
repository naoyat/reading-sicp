(require "./lib")
(require "./2-2")

(define (make-rectangle w h)
  (cons w h))
(define (width r)
  (car r))
(define (height r)
  (cdr r))

(define (perimeter r)
  (+ (* (width r) 2)
     (* (height r) 2)))
(define (area r)
  (* (width r) (height r)))

;; other implimentation
(define (make-rectangle p0 p1 p2 p3)
  (let ((seg1 (make-segment p0 p1))
        (seg2 (make-segment p1 p2)))
    (cons seg1 seg2)))
(define (width r)
  (segment-length (car r)))
(define (height r)
  (segment-length (cdr r)))

(define (segment-length seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (sqrt (+ (square (- (x-point start) (x-point end)))
             (square (- (y-point start) (y-point end)))))))

