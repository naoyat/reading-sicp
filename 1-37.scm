(define (cont-frac1 n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (cont-frac2 n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter (- k 1)
        (/ (n k) (d k))))

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))


(define one (lambda (i) 1.0))

(define (test n)
  (define (iter k)
    (cond ((> k n)  ;end
           (display "//")
           (newline))
          (else
           (display k)
           (display " ")
           (display (cont-frac (lambda (i) 1.0)
                               (lambda (i) 1.0)
                               k))
           (newline)
           (iter (+ k 1)))))
  (iter 1))

(define (test-from-a-to-b proc a b)
  (define (iter i)
    (cond ((> i b)  ;end
           (display "//")
           (newline))
          (else
           (display i)
           (display " ")
           (display (proc i))
           (newline)
           (iter (+ i 1)))))
  (iter a))

;(test-from-a-to-b
; (lambda (k) (cont-frac (lambda (i) 1.0)
;                        (lambda (i) 1.0)
;                        k))
; 1
; 50)
