(require "./binary-tree")

(require "./2-62")

(require "./2-63")
(require "./2-64")

(define (union-set-1 tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1)
                         (tree->list-2 tree2))))

(define (intersection-set-1 tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1)
                                (tree->list-2 tree2))))

