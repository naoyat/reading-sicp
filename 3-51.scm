(require "./stream")
;(require "./stream-nomemo")
;(require "./3-50") ; multi-args stream-map

(define (show x)
  (display-line x)
  x)

;gosh> (define x (stream-map show (stream-enumerate-interval 0 10)))
;
;0x
;gosh> (stream-ref x 5)
;
;1
;2
;3
;4
;55
;gosh> (stream-ref x 7)
;
;6
;77
;gosh> 
