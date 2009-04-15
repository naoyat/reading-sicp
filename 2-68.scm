(require "./234huffman")
;(require "./2-67") ;sample-tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(A D A B B C A))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (iter branch)
;    (print "iter " symbol " in " (symbols branch))
    (if (leaf? branch)
        '()
        (if (memq symbol (symbols (left-branch branch)))
            (cons 0 (iter (left-branch branch)))
            (cons 1 (iter (right-branch branch))))
        ))
  (if (memq symbol (symbols tree))
      (iter tree)
      (error "bad symbol -- UNKNOWN SYMBOL" symbol)))
