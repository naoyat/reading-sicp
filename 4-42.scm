;;
;; 問題4.42 - 嘘つきパズル
;;
(use srfi-42) ; 先行評価的内包表記

(define (distinct? items)
  (cond [(null? items)  #t]
		[(null? (cdr items)) #t]
		[(member (car items) (cdr items)) #f]
		[else (distinct? (cdr items))]
		))

(define (!= x y) (not (= x y)))
(define (xor x y) (or (and x (not y))
					  (and (not x) y)
					  ))

(map print
 (list-ec (: betty 1 6) (: ethel 1 6) (: joan 1 6) (: kitty 1 6) (: mary 1 6)
		  (if (distinct? (list betty ethel joan kitty mary)))

;		  (or (and (= kitty 2) (!= betty 3))
;			  (and (!= kitty 2) (= betty 3)))
;		  (or (and (= ethel 1) (!= joan 2))
;			  (and (!= ethel 1) (= joan 2)))
;		  (or (and (= joan 3) (!= ethel 5))
;			  (and (!= joan 3) (= ethel 5)))
;		  (or (and (= kitty 2) (!= mary 4))
;			  (and (!= kitty 2) (= mary 4)))
;		  (or (and (= mary 4) (!= betty 1))
;			  (and (!= mary 4) (= betty 1)))

		  (if (xor (= kitty 2) (= betty 3)))
		  (if (xor (= ethel 1) (= joan 2)))
		  (if (xor (= joan 3) (= ethel 5)))
		  (if (xor (= kitty 2) (= mary 4)))
		  (if (xor (= mary 4) (= betty 1)))

		  (list (list 'betty betty)
				(list 'ethel ethel)
				(list 'joan joan)
				(list 'kitty kitty)
				(list 'mary mary))
		  ))
