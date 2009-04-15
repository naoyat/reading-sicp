(require "./binary-tree")

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
;; (append 左再帰 (entry . 右再帰))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
;  (trace copy-to-list)
  (copy-to-list tree '()))

;; (ctl 左 (entry . (ctl 右 結果))
;; たぶん右から解いて行く
;; 真ん中より右: result = (entry . 右枝をリスト化したもの) を持って、左へGo
; 
;; なくなったら result

;;b
(define n1 (make-tree 1 '() '()))
(define n3 (make-tree 2
                      (make-tree 1 '() '())
                      (make-tree 3 '() '())
                      ))
(define n7 (make-tree 4
                      (make-tree 2
                                 (make-tree 1 '() '())
                                 (make-tree 3 '() '())
                                 )
                      (make-tree 6
                                 (make-tree 5 '() '())
                                 (make-tree 7 '() '())
                                 )
                      ))
(define n15 (make-tree 8
                       (make-tree 4
                                  (make-tree 2
                                             (make-tree 1 '() '())
                                             (make-tree 3 '() '())
                                             )
                                  (make-tree 6
                                             (make-tree 5 '() '())
                                             (make-tree 7 '() '())
                                             )
                                  )
                       (make-tree 12
                                  (make-tree 10
                                             (make-tree 9 '() '())
                                             (make-tree 11 '() '())
                                             )
                                  (make-tree 14
                                             (make-tree 13 '() '())
                                             (make-tree 15 '() '())
                                             )
                                  )
                       ))

;(use slib)
;(require 'trace)
;(trace tree->list-1)
;(trace tree->list-2)
;(trace copy-to-list)