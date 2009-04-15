;(require "./ch4")
;(use util.combinations)
(use srfi-42) ; 先行評価的内包表記

;(define fathers '(Moore Downing Hall barnacle Parker))
(define daughters '(mary-ann lorna melissa rosalind gabrielle))

(define (!= x y) (not (= x y)))

(define (distinct? items)
  (cond [(null? items)  #t]
		[(null? (cdr items)) #t]
		[(member (car items) (cdr items)) #f]
		[else (distinct? (cdr items))]
		))

(map print
(list-ec (: moore-daughter 0 5) (: downing-daughter 0 5) (: hall-daughter 0 5) (: barnacle-daughter 0 5) (: parker-daughter 0 5)
		 (: moore-yacht 0 5) (: downing-yacht 0 5) (: hall-yacht 0 5) (: barnacle-yacht 0 5) (: parker-yacht 0 5)

		 (if (= moore-daughter 0)) ; mary-ann

		 (if (= barnacle-yacht 4)) ; gabrielle
		 (if (= moore-yacht 1)) ; lorna
		 (if (= hall-yacht 3)) ; rosalind
		 (if (= downing-yacht 2)) ; melissa
		 (if (= barnacle-daughter 2)) ; melissa

		 (if (distinct? (list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
		 (if (distinct? (list moore-yacht downing-yacht hall-yacht barnacle-yacht parker-yacht)))

		 (if (or (and (= moore-daughter 4) (= moore-yacht parker-daughter))
				 (and (= downing-daughter 4) (= downing-yacht parker-daughter))
				 (and (= hall-daughter 4) (= hall-yacht parker-daughter))
				 (and (= barnacle-daughter 4) (= barnacle-yacht parker-daughter))
				 (and (= parker-daughter 4) (= parker-yacht parker-daughter))
				 ))
		 (if (!= moore-daughter moore-yacht))
		 (if (!= downing-daughter downing-yacht))
		 (if (!= hall-daughter hall-yacht))
		 (if (!= barnacle-daughter barnacle-yacht))
		 (if (!= parker-daughter parker-yacht))
			
		 (cond [(= moore-daughter 1) 'Moore]
			   [(= downing-daughter 1) 'Downing]
			   [(= hall-daughter 1) 'Hall]
			   [(= barnacle-daughter 1) 'Barnacle]
			   [(= parker-daughter 1) 'Parker]
			   )
		 
;		 (list (list 'moore (list-ref daughters moore-daughter))
;			   (list 'downing (list-ref daughters downing-daughter))
;			   (list 'hall (list-ref daughters hall-daughter))
;			   (list 'barnacle (list-ref daughters barnacle-daughter))
;			   (list 'parker (list-ref daughters parker-daughter))
		 ))
;("Mary Ann Moore" <"Moore" - "Lorna"
;                   "Downing" - "Melissa" 
;                   "Hall" - "Rosalind"
;"Melissa"	<	   "Barnacle (Hood)" - Gabrielle
;		x	<	   "Parker"
;				   
;"Gabrielle" <       * - x
;
