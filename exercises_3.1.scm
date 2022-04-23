;;;  3.1.1
(let ([x (memv 'a ls)])
  (and x (memv 'b x)))
↓
(let ([x (memv 'a ls)])
  (if x (memv 'b x) #f))
↓
(lambda (x)
  (if x (memv 'b x) #f)
  (memv 'a ls))


;;; 3.1.2
(or (memv x '(a b c)) (list x))
↓
(let ([t (memv x '(a b c))] (if t t (or (list x)))))
↓
(lambda (x)
  (lambda (t)
    (if t t (or (list x)))
    (memv x '(a b c))))


;;; 3.1.3
(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...) (let () e1 e2 ...)]
    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
     (let ((x1 v1))
       (let* ((x2 v2) ...) e1 e2 ...))]))


;;; 3.1.4
(define-syntax when
  (syntax-rules ()
    [(_ t e1 e2 ...) (if (t) (begin e1 e2 ...) (e1))]))

(define-syntax unless
  (syntax-rules ()
    [(_ t e1 e2 ...) (when (not t) e1 e2 ...)]))
