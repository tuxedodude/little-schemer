#lang racket

(require rackunit)

(define (atom? x)
  (nor (pair? x) (null? x)))

(define nil '())

(define (pick n lat)
  (if (null? lat) '()
      (if (= 1 n) (car lat)
          (pick (- n 1) (cdr lat)))))

(check-equal? (pick 1 '(1 2 3 4)) 1)
(check-equal? (pick 2 '(1 2 3 4)) 2)
(check-equal? (pick 4 '(1 2 3 4)) 4)

(define (looking a lat)
  (define (keep-looking x lat)
    (if (number? x)
        (keep-looking (pick x lat) lat)
        (eq? a x)))
  (keep-looking (car lat) lat))

(check-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
(check-false (looking 'caviar '(6 2 grits caviar 5 7 3)))

(define (shift x)
  (list (caar x)
        (list (cadar x)
              (cadr x))))

(check-equal? (shift '((a b) (c d)))
              '(a (b (c d))))
(check-equal? (shift '((a b) c))
              '(a (b c)))

(define (a-pair? x)
  (cond
    [(atom? x) #f]
    [(null? x) #f]
    [(null? (cdr x)) #f]
    [(null? (cddr x)) #t]
    [else #f]))

(define first car)
(define second cadr)
(define (build s1 s2) (cons s1 (cons s2 '())))

(define (align pora)
  (cond
    [(atom? pora) pora]
    [(a-pair? (first pora))
     (align (shift pora))]
    [else
     (build
      (first pora)
      (align (second pora)))]))

(define (weight* x)
  (if (atom? x) 1
      (+ (* (weight* (first x)) 2)
         (weight* (second x)))))

(check-equal? (weight* '((a b) c)) 7)
(check-equal? (weight* '(a (b c))) 5)

(define (collatz n)
  (printf "~a~n" n)
  (if (= n 1) 1
      (if (even? n)
          (collatz (/ n 2))
          (collatz (+ 1 (* 3 n))))))

(define (ackermann n m)
  (cond
    [(zero? n) (+ 1 m)]
    [(zero? m) (ackermann (- n 1) 1)]
    [else
     (ackermann (- n 1)
                (ackermann n (- m 1)))]))

(define (eternity x)
  (eternity x))

((lambda (l)
   (if (null? l) 0
       (add1 (eternity (cdr l))))) '())

(define length0
  (lambda (l)
    (if (null? l) 0
        (add1 (eternity (cdr l))))))

((lambda (l)
   (if (null? l) 0
       (+ 1 (length0 (cdr l))))) '(1))

((lambda (length)
   (lambda (l)
     (if
      (null? l) 0
      (+ 1 (length (cdr l)))))) eternity)

([lambda (mk-length)
   (mk-length eternity)]
 [lambda (length)
   (lambda (l)
     (if (null? l) 0
         (+ 1 (length (cdr l))))) ] )

[(lambda (f)
   (f eternity)) (lambda (LENGTH)
            (lambda (l)
              (if (null? l) 0
                  (+ 1 (LENGTH (cdr l))))))]


(((λ (mk-length)
    (mk-length mk-length))
  (λ (mk-length)
    (λ (l)
      (if (null? l) 0
          (+ 1 ((mk-length eternity)
                (cdr l))))))) '(apples))