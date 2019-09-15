#lang racket

(require rackunit)

; Chapter 4 of Little Schemer: Cons the Magnificent
; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

; Is 14 an atom? Yes
(check-true (atom? 14))

; Is (atom? n) true or false
; where n is 14 ?
; True
(let ((n 14))
  (check-true (atom? n)))

(check-true (number? -3))
(check-true (number? 3.14159))

(define add1
  (lambda (n)
    (+ 1 n)))

; What is (add1 n) where n is 67?
; 68.
(let ((n 67))
  (check-true (= (add1 n) 68)))

; What is (add1 67) ?
; 68
(check-equal? (add1 67) 68)

(define sub1
  (lambda (n) (- n 1)))

(check-equal? (sub1 5) 4)

; What is (sub1 0)
; No answer -- we only consider nonnegative numbers.

(check-true (zero? 0))
(check-false (zero? 1492))

(define o+
  (lambda (n m)
    (if (zero? m) n
        (add1 (o+ n (sub1 m))))))

; What is (add 46 12)?
; 58
(check-equal? (o+ 46 12) 58)

; Try to write the function o-
; Hint: use sub1
(define o-
  (lambda (A B)
    (if (zero? B) A
        (sub1 (o- A (sub1 B))))))

(check-equal?
 (o- 10 5) 5)
(check-equal? (o- 10 10) 0)

(define is-number?
  (lambda (x)
    (and (atom? x)
         (number? x)
         (or (zero? x)
             (is-number? (sub1 x))))))

(define tup?
  (lambda (tup)
    (if (null? tup) #t
        (and (is-number? (car tup))
             (tup? (cdr tup))))))

(check-true (tup? '(2 11 3 79 47 6)))
(check-true (tup? '(8 555 5555)))
(check-false (tup? '(1 2 8 apple 4 3)))
(check-false (tup? '(3 (7 4) 13 9)))

(define addtup
  (lambda (tup)
    (if (null? tup) 0
        (o+ (car tup)
            (addtup (cdr tup))))))

(check-equal? (addtup '(3 5 2 8)) 18)
(check-equal? (addtup '(15 6 7 12 3)) 43)

(define o*
  (lambda (n m)
    (if (zero? m) 0
        (o+ n (o* n (sub1 m))))))

(check-equal? (o* 5 5) 25)
(check-equal? (o* 12 3) 36)

(define tup+
  (lambda (tup1 tup2)
    (if (or (null? tup1)
            (null? tup2))
        '()
        (cons
         (o+ (car tup1) (car tup2))
         (tup+ (cdr tup1) (cdr tup2))))))

(check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(check-true (o> 2 1))
(check-false (o> 0 0))
(check-true (o> 1 0))
(check-false (o> 0 1))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (o< (sub1 n) (sub1 m))))))

(check-true (o< 0 1))
(check-true (o< 1 2))
(check-false (o< 0 0))
(check-false (o< 1 0))

(define o=
  (lambda (n m)
    (nor (o< n m)
         (o> n m))))

(check-true (o= 0 0))
(check-false (o= 1 0))
(check-false (o= 0 1))
(check-true (o= 10 10))

(define o^
  (lambda (n m)
    (if (zero? m) 1
        (o* n (o^ n (sub1 m))))))

(check-equal? (o^ 1 1) 1)
(check-equal? (o^ 2 3) 8)
(check-equal? (o^ 5 3) 125)

(define o/
  (lambda (n m)
    (if (o< n m) 0
        (add1
         (o/ (- n m) m)))))

(check-equal? (o/ 10 5) 2)
(check-equal? (o/ 11 5) 2)
(check-equal? (o/ 15 4) 3)

(define leength
  (lambda (lat)
    (if (null? lat) 0
        (add1 (leength (cdr lat))))))

(check-equal? (leength '()) 0)
(check-equal? (leength '(1 2 3 4)) 4)

(define pick
  (lambda (n lat)
    (if (null? lat) '()
        (if (zero? (sub1 n)) (car lat)
            (pick (sub1 n) (cdr lat))))))

(check-equal? (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)

(define rempick
  (lambda (n lat)
    (if
      (zero? (sub1 n))
      (cdr lat)
      (cons (car lat)
            (rempick (sub1 n) (cdr lat))))))

(let ((n 3) (lat '(hotdogs with hot mustard)))
  (check-equal?
   (rempick n lat)
   '(hotdogs with mustard)))

(check-true (number? 76))

; number? is a primitive; cannot write number? in terms of
; other primitives.

(define remove-if
  (lambda (match? lat)
    (if
     (null? lat) '()
     (if (match? (car lat))
         (remove-if match? (cdr lat))
         (cons (car lat)
               (remove-if match? (cdr lat)))))))

(define no-nums
  (curry remove-if number?))

(check-equal? (no-nums '(5 pears 6 prunes 9 dates))
              '(pears prunes dates))

(define all-nums
  (curry remove-if
         (lambda (a) (and (atom? a) (not (number? a))))))

(check-equal? (all-nums '(5 pears 6 prunes 9 dates))
              '(5 6 9))

(define eqan?
  (lambda (a1 a2)
    (and (atom? a1) (atom? a2))
    (if (and (number? a1) (number? a2))
        (= a1 a2)
        (eq? a1 a2))))

(check-true (eqan? 'a1 'a1))
(check-true (eqan? 2 2))
(check-false (eqan? 'a 1))
(check-false (eqan? '() 'a))
(check-false (eqan? '() 1))

(define occur?
  (lambda (a lat)
    (if (null? lat) 0
        (if (eqan? a (car lat))
            (+ 1 (occur? a (cdr lat)))
            (occur? a (cdr lat))))))

(check-equal?
 (occur? 'a '(a b c 9 2 a 1 a a))
 4)

(check-equal? (occur? 'a '()) 0)
(check-equal? (occur? 'a '(1 2 3 4)) 0)

(define (one? n)
  (= n 1))

(define (rempick-2 n lat)
  (if (null? lat) '()
      (if (one? n) (cdr lat)
          (cons (car lat) (rempick-2 (sub1 n) (cdr lat))))))

(check-equal? (rempick-2 1 '(a b c)) '(b c))
(check-equal? (rempick-2 2 '(a b c)) '(a c))
(check-equal? (rempick-2 3 '(a b c)) '(a b))

(displayln "chapter complete")