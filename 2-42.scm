(require "./lib")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil) ;空リスト
(define (adjoin-position row col rest)
  (cons (list row col) rest))

(define (safe? k positions)
  (let ((kth (car positions)))
    (define (iter rest)
      (cond ((null? rest) #t)
            ((conflicts? (car rest) kth) #f)
            (else (iter (cdr rest)))))
    (iter (cdr positions))))

(define (conflicts? a b)
  (let ((dx (abs (- (car a) (car b))))
        (dy (abs (- (cadr a) (cadr b)))))
      (cond ((= dx 0) #t)
            ((= dy 0) #t)
            ((= dx dy) #t)
            (else #f))
      ))


(define (show position-set)
  (define (p positions)
    (print positions))
  (map p position-set))