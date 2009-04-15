;(require "./stream")

(define (stream-map proc . argstreams) ; 引数は１つ以上の stream
  (if (stream-null? (car argstreams)) ; 最初の stream が空なら<終わり>
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; 1st's
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) ; 2nd以降
; 出力ストリームの nth = 入力各ストリームの nth を proc で演算した結果

;test
;; (define s (stream-map sqrt))
;(define e (stream-map sqrt the-empty-stream))
;(define s (stream-map sqrt integers))
;(define t (stream-map + integers integers integers))
;(display-stream (stream-map sqrt integers))