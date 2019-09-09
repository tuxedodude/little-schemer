#lang racket
(require rackunit)

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

; chapter 1

; p. 3
(check-true (atom? 'atom))
(check-true (atom? 'turkey))
(check-true (atom? 1492))
(check-true (atom? 'u))
(check-true (atom? '*abc$))

(check-true (list? '(atom)))
(check-true (list? '(atom turkey or)))
; note: "(atom turkey) or" can't actually be expressed in scheme.
(check-true (list? '((atom turkey) or)))

(define (sexpr? s)
  (or (atom? s) (list? s)))

(check-true (sexpr? 'xyz))
(check-true (sexpr? '(x y z)))
(check-true (sexpr? '((x y) z)))
(check-true (list? '(how are you doing so far)))

; how many s-expressions are in the list...?
(check-equal? (length '(how are you doing so far)) 6)
(check-true (list? '(((how) are) ((you) (doing so)) far)))

; how many s-expressions are in the list (((how) are) ((you) (doing so)) far) ?
(check-equal?
 (length '(((how) are) ((you) (doing so)) far))
 3)

; Q: Is it true that () is a list? A: Yes, because it contains zero S-expressions enclosed by parentheses.
(check-true (list? '()))

; Q: Is it true that this is an atom?
; A: No, because () is just a list.
(check-false (atom? '()))

; Q: Is it true that (() () () ()) is a list? A: Yes...
(check-true (list? '(() () () ())))

; Q: What is the <car of l> where l is the argument (a b c)?
; A: a, because a is the first atom of this list.
(let [(l '(a b c))]
(check-eq? (car l) 'a))

; Q: What is the car of l where l is ((a b c) x y z)?
; A: (a b c), because (a b c) is the first S-expression of this non-empty list.
(let ((l '((a b c) x y z)))
  (check-equal? (car l) '(a b c)))

; Q: What is the *car* of *l* where *l* is _hotdog_?
; A: You cannot ask for the car of an atom.
(check-exn exn:fail? (lambda () (car 'hotdog)))

; Q: What is the car of l where l is ()?
; A: You cannot ask for the car of an empty list.
(check-exn exn:fail? (lambda () (car '())))

; The Law of Car: the primitive car is defined only for non-empty lists.
