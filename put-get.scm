(define (put-get-gen)
  (define (make-record key1 key2 value)
    (list key1 key2 value))
  (define (key1-record rec) (car rec))
  (define (key2-record rec) (cadr rec))
  (define (value-record rec) (caddr rec))
  (define (match-record? rec key1 key2)
    (and (equal? (key1-record rec) key1)
         (equal? (key2-record rec) key2)))
;    (and (eq? (key1-record rec) key1)
;         (eq? (key2-record rec) key2)))

  (define table '())

  (list
   (lambda (op type item) ; put
     (define (delete-from tbl key1 key2 result)
       (if (null? tbl)
           result
           (let ((rec (car tbl)))
             (if (match-record? rec key1 key2)
                 (delete-from (cdr tbl) key1 key2 result)
                 (delete-from (cdr tbl) key1 key2 (cons (car tbl) result))
                 ))))
     (let ((new-rec (make-record op type item)))
       (set! table (cons new-rec (delete-from table op type '())))
       new-rec))

   (lambda (op type) ; get
     (define (select-from tbl key1 key2)
       (if (null? tbl)
           #f ;;;'()
           (let ((rec (car tbl)))
             (if (match-record? rec key1 key2)
                 (value-record rec)
                 (select-from (cdr tbl) key1 key2))))
       )
     (select-from table op type))

   (lambda () table)
   ))

(define put-get (put-get-gen))
(define put (car put-get))
(define get (cadr put-get))
(define table (caddr put-get))
