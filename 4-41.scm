;;
;; 問題4.41 - 多住居パズルを解く通常の Scheme プログラムを書け
;;
(use srfi-42) ; 先行評価的内包表記

(define (distinct? items)
  (cond [(null? items)  #t]
		[(null? (cdr items)) #t]
		[(member (car items) (cdr items)) #f]
		[else (distinct? (cdr items))]
		))

(map print
 (list-ec (: baker 1 6) (: cooper 1 6) (: fletcher 1 6) (: miller 1 6) (: smith 1 6)

		  (if (distinct? (list baker cooper fletcher miller smith)))
		  (not (= baker 5))
		  (not (= cooper 1))
		  (not (= fletcher 5))
		  (not (= fletcher 1))
		  (if (> miller cooper))
		  (not (= 1 (abs (- smith fletcher))))
		  (not (= 1 (abs (- fletcher cooper))))

		  (list (list 'baker baker)
				(list 'cooper cooper)
				(list 'fletcher fletcher)
				(list 'miller miller)
				(list 'smith smith))
		  ))
