#lang racket

(require rackunit)

(define (atom? x)
  (nor (pair? x) (null? x)))

(define nil '())

(define (rember-f test? a l)
  (define (R l)
    (if (null? l) '()
        (if (test? (car l) a) (cdr l)
            (cons (car l) (R (cdr l))))))
  (R l))

(define (eq?-c a)
  (curry eq? a))

(define (mk-rember-f test?)
  (lambda (a l)
    (if (null? l) '()
        (if (test? (car l) a) (cdr l)
            (cons (car l)
                  ((mk-rember-f test?) a (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (seqS new old l)
  (cons new l))

(define (seqRemove new old l) l)

(define (insert-g seq)
  (lambda (new old l)
    (if (null? l) '()
        (if (eq? old (car l))
            (seq new old (cdr l))
            (cons (car l)
                  ((insert-g seq) new old (cdr l)))))))


(define optable
  `((* . ,*)
    (+ . ,+)
    (- . ,-)
    (^ . ,expt)))

(define (atom-to-function x)
  (cdr (assoc x optable)))

(define operator car)

; find value of 
(define (value expr)
  (if (atom? expr) expr
      ((atom-to-function
        (operator expr))
       (value (cadr expr))
       (value (caddr expr)))))

(check-equal? (value '(* 2 4)) 8)
(check-equal? (value '(+ (* 2 4) 1)) 9)

(define (filter* keep? l)
  (define (filter* l)
    (if (null? l) '()
        (let [(A (car l))
              (B (cdr l))]
          [if (atom? A)
              [if (keep? A)
                  (cons A (filter* B))
                  (filter* B)]
              (cons
               (filter* A)
               (filter* B))] )))
  (filter* l))

(define evens-only* (curry filter* even?))

(check-equal?
 (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
 '((2 8) 10 (() 6) 2))

(define (a-friend x y)
  (null? y))

(define (multirember&co a lat col)
  (let [(recur (lambda (f)
                 (multirember&co a (cdr lat) f)))
        (match? (curry eq? a))]
    (cond
      [(null? lat) (col nil nil)] ; base case, so later calls to col will build a list
      [(match? (car lat))
       (recur
           (lambda (x y) ; on match, cons the match onto x
             (col x (cons (car lat) y))))]
      [else
       (recur
           (lambda (x y) ; on match, cons the match onto y
             (col (cons (car lat) x) y)))])))
(let [(a 'tuna)
      (b '(tuna fish sun fish garfish tuna catfish))]
  (check-equal?
   (multirember&co a b
                   (lambda (x y) x))
   '(fish sun fish garfish catfish))

  (check-equal?
   (multirember&co a b
                   (lambda (x y) y))
   '(tuna tuna)))

; simpler to understand, because it simply conses a list.
; also, it does not accumulate a series of unevaluated lambdas
; that unroll at the end.
(define (mr&co a lat)
  (define (M lat x y)
    (cond
      [(null? lat) (list x y)]
      [(eq? (car lat) a)
       (M (cdr lat) x (cons (car lat) y))]
      [else
       (M (cdr lat) (cons (car lat) x) y)]))
  (M lat '() '()))


(define (multiinsertLR new oldL oldR lat)
  (let ((L? (curry eq? oldL))
        (R? (curry eq? oldR)))
    (let [(recurse (lambda () (multiinsertLR new oldL oldR (cdr lat))))]
      (cond
        [(null? lat) '()]
        [(L? (car lat))
         (cons new (cons oldL (recurse)))]
        [(R? (car lat))
         (cons oldR (cons new (recurse)))]
        [else
         (cons (car lat)
               (recurse))]))))
(check-equal?
 (multiinsertLR 'tuna 'fish 'sandwich '(I love fish sandwich))
 '(I love tuna fish sandwich tuna))

; collect the new list into argument x,
; the count of left insertions into y,
; the count of right insertions into z.
(define (multiinsertLR&co new oldL oldR lat col)
  (let [(L? (curry eq? oldL))
        (R? (curry eq? oldR))
        (A '())
        (recurse (lambda (f)
                   (multiinsertLR&co new oldL oldR (cdr lat) f)))]
    (cond
      [(null? lat) (col '() 0 0)]
      [(L? (car lat))
       (recurse (lambda (x y z)
                  (col (cons new (cons oldL x))
                       (+ y 1)
                       z)))]
      [(R? (car lat))
       (recurse (lambda (x y z)
                  (col (cons oldR (cons new x))
                       y
                       (+ z 1))))]
      [else
       (recurse (lambda (x y z)
                  (col (cons (car lat) x)
                       y z)))])))

; This is cool conceptually, but it's also fucking ugly,
; and it's inflexible.
(define (evens-only*&co l col)
  (cond
    [(null? l) (col '() 1 0)]
    [(atom? (car l))
     (if (even? (car l))
         (evens-only*&co (cdr l)
                         (lambda (x y z)
                           (col (cons (car l) x)
                                (* y (car l))
                                z)))
         ; else odd number
         (evens-only*&co (cdr l)
                         (lambda (x y z)
                           (col x y (+ z
                                       (car l))))))]
    [else
     (evens-only*&co (car l)
                     (lambda (x y z)
                       (evens-only*&co (cdr l)
                                       (lambda (x1 y1 z1)
                                         (col (cons x x1)
                                              (* y y1)
                                              (+ z z1))))))]))

(let [(l '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
      (f (lambda (x y z)
                    (list z y x)))]
  (check-equal? (evens-only*&co l f)
                '(38 1920 ((2 8) 10 (() 6) 2))))