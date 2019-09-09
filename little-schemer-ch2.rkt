#lang racket

(require rackunit)

; Chapter 2 of Little Schemer.
; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

; Chapter 2, p. 15

(define lat?
  (lambda (l)
    (cond [(null? l) #t]
          [(atom? l) #f]
          [(list? l)
           (and (atom? (car l))
                (lat? (cdr l)))]
          [else #f])))

(define book-lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; Q: True or false: (lat? l)
; where l is (Jack Sprat could eat no chicken fat)
; A: True, because each S-expression in l is an atom.
(check-true (lat? '(Jack Sprat could eat no chicken fat)))

; Q: True or false: (lat? l)
; where l is ((Jack) Sprat could eat no chicken fat)
; A: False, since (car l) is a list.
(check-false (lat? '((Jack) Sprat could eat no chicken fat)))

; Q: True or false: (lat? l)
; where
; l is (Jack (Sprat could) eat no chicken fat)
; A: False, since one of the s-expressions in l is a list.
(check-false (lat? '(Jack (Sprat could) eat no chicken fat)))

; Q: True or false: (lat? l)
; where l is ()
; A: True, because it does not contain a list
; [trivially true, if we define a lat as a list containing no lists....]
(check-true (lat? '()))

; Q: True or false: a lat is a list of atoms.
; A: True! Every lat is a list of atoms!

; Q: What is the value of (lat? l)
; where l is the argument (bacon and eggs)
; A: #t
(check-true (lat? '(bacon and eggs)))

; ... skipping tedious illumination of Scheme basics here...

; Q: What is the value of (lat? l)
; where l is now (bacon (and eggs))
; A: #f, because l contains a list.
(check-false (lat? '(bacon (and eggs))))

; Q: What does (or ...) do?
; A: It asks if the first argument is true.
; If it is true, then it skips evaluating the second argument.
; If the first argument is false, then and answers with
; the value of the the second expression.

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or
        (eq? a (car lat))
        (member? a (cdr lat)))))))

; higher order...
(define (member-2? a lat)
  (ormap (curry eq? a) lat))

; Q: Is it true or false that a is a member of lat
; where a is tea
; and lat is (coffee tea or milk) ?
; A: True
(let [(a 'tea)
      (lat '(coffee tea or milk))]
  (check-true
   (member? a lat)))

; First commandment: always ask null? as the first question in expressing any function.

; p. 28
; Series of (member?) questions....
(check-true (member? 'meat '(meat gravy)))
(check-true (member? 'meat '(potatoes and meat gravy)))
(check-false (member? 'liver '(bagels and lox)))

(print "This space for doodling")