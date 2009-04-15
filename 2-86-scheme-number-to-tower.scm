scheme-number を塔に組み込みたいが、scheme-number はこれらすべてを内包しているので厄介。

とりあえず、scheme-number に、塔内の適切な型をつける関数を作る。
>||
(define (scheme-number->tower x)
  (cond ((fixnum? x)   (make-integer x))
        ((flonum? x)   (make-real x))
        ((rational? x) (attach-tag 'rational (get-nearest-rational x)))
        ((complex? x) (make-complex-from-real-imag (real-part x) (imag-part x)))
                         ; real-part, imag-part は R5RS のもの
        (else (error "unknown type -- SCHEME-NUMBER->TOWER" x))))
||<
と行きたいところだが、Gauche では有理数がサポートされていないので
>||
(define (scheme-number->tower x)
  (cond ((fixnum? x) (make-integer x))
        ((flonum? x)  ;; Gauche用
         (let ((nearest-rational (get-nearest-rational x)))
           (if (< (cdr nearest-rational) 1000)
               (attach-tag 'rational nearest-rational)
               (attach-tag 'real x))))
        ((complex? x) (make-complex-from-real-imag (real-part x) (imag-part x)))
                         ; real-part, imag-part は R5RS のもの
        (else (error "unknown type -- SCHEME-NUMBER->TOWER" x))))
||<
※real-part, imag-part は R5RS とかぶっているのでここでは使えない。
>||
gosh> (scheme-number->tower 1)
(integer . 1)
gosh> (scheme-number->tower 1/3)
(rational 1.0 . 3)
gosh> (scheme-number->tower 2/4)
(rational 1.0 . 2)
gosh> (scheme-number->tower 1.5) ;; 
(rational 3.0 . 2)
gosh> (scheme-number->tower 1.42857)
(rational 10.0 . 7)
gosh> (scheme-number->tower 3.14159)
(real . 3.14159)
||<
こんな具合。
