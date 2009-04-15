(require "./put-get")

(define (attach-tag type-tag contents)
  (cons type-tag contents))

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
;    (print 'get " " op " " type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
;;
(define (install-section9-package)
  ;;内部手続き
  (define (make-record name address salary)
    (list 'employee
          name
          (attach-tag 'address address)
          (attach-tag 'salary salary)))

  (define (name record) (cadr record))
  (define (address record) (contents (caddr record)))
  (define (salary record) (contents (cadddr record)))

  (define (get-record branch-employee-file employee-name)
    (define (iter records)
      (cond ((null? records) #f)
            ((eq? employee-name (name (contents (car records)))) (car records))
            (else (iter (cdr records)))))
    (iter branch-employee-file))

  ;;システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'section9 x))
  (put 'make-file 'section9
       (lambda (record-list) (tag record-list)))
  (put 'make-record 'section9 
       (lambda (name address salary)
         (tag (make-record name address salary))))

  (put 'get-name 'section9 name)
  (put 'get-address '(section9) address)
  (put 'get-salary '(section9) salary)
;  (put 'salary-record 'section9 salary-record)

  (put 'get-record 'section9 get-record)

  'done)

(define (make-section9-file)
  (define make-file (get 'make-file 'section9))
  (define make-record (get 'make-record 'section9))
  (make-file (list (make-record 'aramaki 'niihama 1000000)
                   (make-record 'kusanagi 'niihama 900000)
                   (make-record 'batou 'niihama 800000)
                   (make-record 'togusa 'niihama 700000)
                   (make-record 'ishikawa 'niihama 600000)
                   (make-record 'saito 'niihama 500000)
                   (make-record 'paz 'niihama 400000)
                   (make-record 'borma 'niihama 300000)
                   (make-record 'tachikoma 'niihama 200000))))

;;

;;a
(define (get-record branch-employee-file employee-name)
  ((get 'get-record (type-tag branch-employee-file)) (contents branch-employee-file) employee-name))

;;b
;(define (get-salary employee-record)
;  ((get 'get-salary (type-tag employee-record)) (contents employee-record)))
(define (get-salary employee-record)
  (apply-generic 'get-salary employee-record))

;;c
(define (find-employee-record file-list employee-name)
  (if (null? file-list)
      #f
      (or (get-record (car file-list) employee-name)
          (find-employee-record (cdr file-list) employee-name))))

(install-section9-package)
(define section9-file (make-section9-file))
;(define all-files (list section9-file))


;;a
(define batou-rec (get-record section9-file 'batou))
;;b
;(define batou-salary (get-salary batou-rec))
;;c
;(define batou-from-all (find-employee-record sample-all-files 'batou))
;;d

(define (install-section10-package)
  ;;内部手続き
  (define (make-record name address salary)
    (list 'name name 'address address 'salary salary))

  (define (name record) (cadr record))
  (define (address record) (cadddr record))
  (define (salary record) (cadr (cddddr record)))

  (define (get-record branch-employee-file employee-name)
    (define (iter records)
      (cond ((null? records) #f)
            ((eq? employee-name (name (contents (car records)))) (car records))
            (else (iter (cdr records)))))
    (iter branch-employee-file))

  ;;システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'section10 x))
  (put 'make-file 'section10
       (lambda (record-list) (tag record-list)))
  (put 'make-record 'section10
       (lambda (name address salary)
         (tag (make-record name address salary))))

  (put 'get-name 'section10 name)
  (put 'get-address '(section10) address)
  (put 'get-salary '(section10) salary)
;  (put 'salary-record 'section9 salary-record)

  (put 'get-record 'section10 get-record)

  'done)

(install-section10-package)

(define beatles-co
  (list
   '(name john address liverpool salary 1800000)
   '(name paul address liverpool salary 1600000)
   '(name george address liverpool salary 1400000)
   '(name ringo address liverpool salary 1200000)
   ))

(define (make-section10-file)
  (define (tag x) (attach-tag 'section10 x))
  (tag (map tag beatles-co)))

(define section10-file (make-section10-file))

(define all-files (list section9-file section10-file))
