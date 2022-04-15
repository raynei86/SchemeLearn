;;; 2.9.1
(define make-counter
  (lambda (init inc)
    (let ([next init])
      (lambda ()
        (let ([temp next] [amount inc]))
        (set! next (+ next inc))
        temp))))


;;; 2.9.2
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (case msg
         [(empty? mt?) (null? ls)]
         [(push!) (set! ls (cons (car args) ls))]
         [(top) (car ls)]
         [(pop!) (set! ls (cdr ls))]
         [else "oops"])))))


;;; 2.9.3
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (case msg
         [(empty? mt?) (null? ls)]
         [(push!) (set! ls (cons (car args) ls))]
         [(pop!) (set! ls (cdr ls))]
         [(set!) (set-car! (list-tail ls (car args)) (cadr args))]
         [(top) (car ls)]
         [(ref) (list-ref ls (car args))]
         [else "oops"])))))


;;; 2.9.4
(define make-stack
  (lambda (n)
    (let ([vec (make-vector n)] [capacity 0])
      (lambda (msg . args)
        (case msg
         [(empty? mt?) (null? vec)]
         [(push!) (vector-set! vec capacity (car args)) (set! capacity (+ capacity 1))]
         [(pop!) (vector-set! vec (- capacity 1) 0) (set! capacity (- n 1))]
         [(set!) (vector-set! vec (car args) (cadr args))]
         [(top) (vector-ref vec (- n 1))]
         [(ref) (vector-ref vec (car args))]
         [else "oops"])))))


;;; 2.9.5
(define emptyq?
  (lambda (q)
    (if (and (eqv? (cadr q) 'ignored) (eqv? (cdar q) '()))
        #t
        #f)))

(define getq
  (lambda (q)
    (if (emptyq? q)
        (assertion-violation 'q "Empty queue")
        (caar q))))

(define delq!
  (lambda (q)
    (if (emptyq? q)
        (assertion-violation 'q "Empty queue")
        (set-car! q (cdar q)))))


;;; 2.9.6
(define make-queue
  (lambda ()
      (cons '() '())))

(define putq!
  (lambda (q v)
    (let ([a (cons v '())])
      (if (null? (car q))
          (begin
            (set-car! q a)
            (set-cdr! q a))
          (begin
            (set-cdr! (cdr q) a)
            (set-cdr! q a))))))

(define getq
  (lambda (q)
    (caar q)))

(define delq!
  (lambda (q)
    (if (eqv? (car q) (cdr q))
        (begin
          (set-car! q '())
          (set-cdr! q '()))
        (set-car! q (cdar q)))))


;;; 2.9.7
(let ([ls (cons 'a '())])
  (set-cdr! ls ls)
  ls)
;;; length doesn't work on circular list


;;; 2.9.8
;;; Official Answer*
(define race
  (lambda (hare tortoise)
    (if (pair? hare)
        (let ([hare (cdr hare)])
          (if (pair? hare)
              (and (not (eq? hare tortoise))
                   (race (cdr hare) (cdr tortoise)))
              (null? hare)))
        (null? hare))))

(define list?
  (lambda (x)
    (race x x)))
