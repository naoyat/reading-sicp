(define rock-leafs
  '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(require "./2-69")
(define rock-tree (generate-huffman-tree rock-leafs))

(define lyrics '(
GET A JOB
SHA NA NA NA NA NA NA NA NA
GET A JOB
SHA NA NA NA NA NA NA NA NA
WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
SHA BOOM))

(define bits (encode lyrics rock-tree))
