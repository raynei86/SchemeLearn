;;; 2.8.2
(define append
  (lambda (lst1 lst2)
    (if (null? lst1)
    lst2
    (cons lst1 (append (cdr lst1) lst2)))))


;;; 2.8.3
(define make-list
  (lambda (n obj)
    (if (= n 0)
        '()
        (cons obj (make-list (- n 1) obj)))))


;;; 2.8.4
(define list-ref
  (lambda (lst n)
    (if (= n 0)
        (car lst)
        (list-ref (cdr lst) (- n 1)))))

(define list-tail
  (lambda (lst n)
    (if (= n 0)
        lst
        (list-tail (cdar lst) (- n 1)))))


;;; 2.8.5
(define shorter?
  (lambda (lst1 lst2)
    (and (not (null? lst2))
         (or (null? lst1)
             (shorter? (cdr lst1) (cdr lst2))))))

(define shorter
  (lambda (lst1 lst2)
    (if (shorter? lst1 lst2) lst2 lst1)))


;;; 2.8.6
(define odd?
  (lambda (n)
    (if (= 0 n)
        #f
        (even? (- n 1)))))

(define even?
  (lambda (n)
    (if (= n 0)
        #t
        (odd? (- n 1)))))


;;; 2.8.7
(define transpose
  (lambda (lst)
    (cons (map car lst) (map cdr lst))))
