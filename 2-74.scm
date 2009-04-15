(require "./put-get")
;employees-record
;: list of
;   ('taro address salary ...)
;  (address of 'taro = ...)
;  (salary of 'taro = ...)
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (find-obj tag items)
  (cond ((null? items) #f)
        ((and (pair? (car items))
              (eq? (caar items) tag)) (car items))
        (else (find-obj tag (cdr items)))))

;bureau1
(define (install-bureau1-package)
  ;;内部手続き

;  (define (make-file rec-list) (tag rec-list))
  (define (get-record file name)
    (define (iter records)
      (cond ((null? records) #f)
            ((eq? name (key-employee (contents (car records)))) (car records))
            (else (iter (cdr records)))))
    (iter file))
 

  (define (make-employee name address salary)
    (list 'employee
          name
          (attach-tag 'address address)
          (attach-tag 'salary salary)))

;  (define (employee? rec)
;    (eq? (car rec) 'employee))
  (define (key-employee rec) (name-employee rec))
  (define (name-employee rec) (cadr rec))
  (define (address-employee rec) (cdr (find-obj 'address rec)))
  (define (salary-employee rec) (cdr (find-obj 'salary rec)))

;;
  (define (tag x) (attach-tag 'bureau1 x))
  ;make
  (put 'make-file 'bureau1 
       (lambda (rec-list) (tag rec-list)))
  (put 'make-employee 'bureau1
       (lambda (name address salary)
         (tag (make-employee name address salary))))
  ;selector
  (put 'key 'bureau1 key-employee)
;  (put 'name 'bureau1 name-employee)
;  (put 'address 'bureau1 address-employee)
;  (put 'salary 'bureau1 salary-employee)
  ;
  (put 'get-record 'bureau1 get-record)

  (put 'get-name 'bureau1 name-employee)
  (put 'get-address 'bureau1 address-employee)
  (put 'get-salary 'bureau1 salary-employee)
)

;;;bureau2
;(define (make-employee-record name address salary)
; (list 'employee name 'address address 'salary salary))
;(define (employee? rec)
;  (eq? (car rec) 'employee))
;(define (key-employee rec) (name rec))
;(define (name-employee rec) (car rec))
;(define (address-employee rec) (cadr (memq 'address rec)))
;(define (salary-employee rec) (cadr (memq 'salary rec)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (print type-tags " " proc)
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

;;
(define (get-record file name)
  ((get 'get-record (type-tag file)) (contents file) name)
;  (apply-generic 'get-record file name)
;  ((get 'get-record 'bureau1) name file)
  ;file:事業所ファイル. bureauを示すタグがないと困る
  )

(define (get-salary rec)
  ((get 'get-salary (type-tag rec)) (contents rec))
;  (apply-generic 'get-salary rec)
  )

(define (find-employee-record name record-files)
  (define (iter files)
    (if (null? files)
        #f
        (let ((rec (get-record (car files) name)))
          (or rec (iter (cdr files))))))
  (iter record-files))

(install-bureau1-package)

(define make-file (get 'make-file 'bureau1))
(define make-employee (get 'make-employee 'bureau1))
(define file1 (make-file (list (make-employee 'batou 'japan 1000000)
                               (make-employee 'aramaki 'japan 2000000))))
(define files (list file1))


(define b (get-record file1 'batou))
