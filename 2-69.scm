(require "./234huffman")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; make-leaf-set '((A 4) (B 2) (C 1) (D 1))
;; (記号 頻度) --> leaf化
(define (successive-merge leaf-set)
;  (print leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                    (cadr leaf-set))
                                    (cddr leaf-set)))))

(define test-leafs
  '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(define sample-leafs
  '((A 4) (B 2) (C 1) (D 1)))

(require "./2-67") ;decode
(require "./2-68") ;encode

(define t (generate-huffman-tree test-leafs))
(define s (generate-huffman-tree sample-leafs))
