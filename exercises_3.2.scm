;;; 3.2.1
(define tail-recur-procs '(list? factorial fibbonaci factor))
(define not-tail-recur-procs '(sum factorial fibbonaci))

;;; 3.2.2
(define factor
  (lambda (n)
    (letrec ([f (lambda (n i)
                  (cond
                   [(>= i n) (list n)]
                   [(integer? (/ n i)) (cons i (f (/ n i) i))]
                   [else (f n (+ i 1))]))])
      (f n 2))))

;;; 3.2.3
;;; This looks a bit stupid...
(let even? ([odd? (lambda (x)
                        (or (= x 0))
                        (even? (- x 1)))] [even? (lambda (x)
                                                  (or (= x 0)
                                                   (odd? (- x 1))))])
  (even? 20))


;;; 3.2.4
(define counter 0)
;;; 117 calls for (fibbonaci 10)
(define fibbonaci
  (lambda (n)
    (let fib ([i n])
      (set! counter (+ counter 1))
      (cond
       [(= i 0) 0]
       [(= i 1) 1]
       [else (+ (fib (- i 1)) (fib (- i 2)))]))))

;;; 10 calls for (fibbonaci 10)
(define fibbonaci
  (lambda (n)
    (if (= n 0)
        0
        (let fib ([i n] [a1 1] [a2 0])
          (set! counter (+ counter 1))
          (if (= i 1)
              a1
              (fib (- i 1) (+ a1 a2) a1))))))



;;; 3.2.5
(define-syntax let
  syntax-rules ()
    [(_ ((x e) ...) e1 e2 ...)
     ((lambda (x ...)
        b1 b2 ...)
      e ...)]
    [(_ name ((x e) ...) e1 e2 ...)
     ((letrec ([name (lambda (x ...) b1 b2 ...)]) f) e ...)])


;;; 3.2.6
;;; Other or definition will make even? no longer tail recursive
;;; Which can exhaust stack space with large inputs


;;; 3.2.7
(define factor
  (lambda (n)
    (let f ([n n] [i 2])
      (set! fac (/ n i))                ;;; Maybe there is a more clever solution to not computer (/ n i) twice
      (cond
       [(>= i n) (list n)]
       [(= (sqrt n) i) n]
       [(integer? fac)
        (cons i (f fac i))]
       [else (f n
                (if (= i 2)             ;;; Again, there is probably a more clever solution, but this works for now
                    (+ i 1)
                    (+ i 2)))]))))

;;; The most important optimization was definitely reducing the amount of time (/ n i) was calculated
