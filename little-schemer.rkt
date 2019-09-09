#lang racket
(require rackunit)

; Chapter 1 of Little Schemer.
; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

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

; p. 6

; Q: What is the car of l where l is (((hotdogs)) (and) (pickle) relish)
; A: ((hotdogs))
(let ((l '(((hotdogs)) (and) (pickle) relish)))
  (check-equal? (car l) '((hotdogs))))

; Q: What is (car (car l)) where l is (((hotdogs)) (and)) ?
; A: (hotdogs)
(let [(l '(((hotdogs)) (and)))]
  (check-equal? (car (car l)) '(hotdogs)))

; Q: What is the cdr of l where l is (a b c)?
; A: (b c)
(check-equal? (cdr '(a b c)) '(b c))

; Q: What is the cdr of l where l is ((a b c) x y z)?
; A: (x y z)
(check-equal? (cdr '((a b c) x y z)) '(x y z))

; Q: What is the cdr of l where l is (hamburger)?
; A: ()
(check-equal? (cdr '(hamburger)) '())

; Q: What is (cdr l) where l is ((x) t r)?
; A: (t r)
(check-equal? (cdr '((x) t r)) '(t r))

; Q: What is (cdr a) where a is hotdogs?
; A: You cannot ask for the cdr of an atom.
(check-exn exn:fail? (lambda () (cdr 'hotdogs)))

; Q: What is (cdr l) where l is ()?
; A: You cannot ask for the cdr of an empty list.
(check-exn exn:fail? (lambda () (cdr '())))

; The Law of Cdr
; The primitive cdr is defined only for non-empty lists.
; The cdr of any non-empty list is always another list.

; Q: What is (car (cdr l)) where  is ((b) (x y) ((c))) ?
; A: (x y),
; because ((x y) ((c))) is (cdr l), and (x y) is the car of (cdr l).
(let [(l '((b) (x y) ((c))))]
  (check-equal? (car (cdr l)) '(x y)))

; Q: What is (cdr (cdr l)) where l is ((b) (x y) ((c))) ?
; A: (((c))), because ((x y) ((c))) is (cdr l),
; and (((c))) is cdr of (cdr l).
(let [(l '((b) (x y) ((c))))]
  (check-equal? (cdr (cdr l)) '(((c)))))

; Q: What is (cdr (car l))
; where l is (a (b (c)) d) ?
; A: No answer, since (car l) is an atom, and you cannot ask the cdr of an atom.
(let [(l '(a (b (c)) d))]
  (check-exn exn:fail? (lambda () (cdr (car l)))))

; Q: What does car take as an argument?
; A: it takes any non-empty list.

; Q: What does cdr take as an argument?
; A: it takes any non-empty list.

; Q: What is the cons of the atom a and the list l,
;    where a is peanut
;    and l is (butter and jelly) ?
; A: (peanut butter and jelly), because cons adds an atom to the front of a list.
(let [(a 'peanut)
      (l '(butter and jelly))]
  (check-equal?
   (cons a l)
   '(peanut butter and jelly)))

; Q: What is the cons of s and l
; where s is (banana and)
; and l is (peanut butter and jelly) ?
; A: ((banana and) peanut butter and jelly),
; because cons adds any S-expression to the front of a list
; (an S-expression is an atom or a list...)
(let [(s '(banana and))
      (l '(peanut butter and jelly))]
  (check-equal?
   (cons s l)
   '((banana and) peanut butter and jelly)))

; Q: What is (cons s l)
; where s is ((help) this),
; and l is (is very ((hard) to learn)) ?
; A: (((help) this) is very ((hard) to learn))
(let [(s '((help) this))
      (l '(is very ((hard) to learn)))]
  (check-equal?
   (cons s l)
   '(((help) this) is very ((hard) to learn))))

; Q: What does cons take as its arguments?
; A: 1. An s-expression and 2. a list.

; Q: What is (cons s l)
; where s is (a b (c))
; and l is () ?
; A: ((a b (c)))
(let [(s '(a b (c)))
      (l '())]
  (check-equal?
   (cons s l)
   '((a b (c)))))

; Q: What is (cons s l)
; where s is a and l is ()?
; A: (a)
(check-equal? (cons 'a '()) '(a))

; Q: What is (cons s l)
; where s is ((a b c))
; and l is b ?
; A: No answer, since the second argument l must be a list.

; In reality, (cons 'a 'b) -> '(a . b), a dotted pair,
; where (car (cons 'a 'b)) is 'a,
; and (cdr (cons 'a 'b)) is 'b.
(check-equal? (cons 'a 'b) '(a . b))
(check-equal? (car '(a . b)) 'a)
(check-equal? (cdr '(a . b)) 'b)
(let [(s '((a b c)))
      (l 'b)]
  (check-equal?
   (cons s l)
   '(((a b c)) . b)))

; Q: What is (cons s l) where s is a and l is b?
; A: No answer, because the second argument of cons must be a list.
; Real talk: '(a . b), unless we redefine cons in our namespace.

; The Law of Cons:
; cons takes two arguments.
; The second argument to cons must be a list. The result is a list.

; Q: What is (cons s (car l))
; where s is a
; and
; l is ((b) c d)
; A: (a b)
(let [(s 'a)
      (l '((b) c d))]
  (check-equal?
   (cons s (car l))
   '(a b)))

; Q: What is (cons s (cdr l))
; where s is a
; and l is ((b) c d) ?
; A: (a c d).
; Why?
; Because (cdr l) is (c d),
; and cons inserts its first argument as the first element of the list.
(let [(s 'a)
      (l '((b) c d))]
  (check-equal?
   (cons s (cdr l))
   '(a c d)))

; Q: Is is true that the list l is the null list where l is () ?
; A: Yes, because it is the list composed of zero s-expressions.
; This question can also be written (null? l)
(let [(l '())]
  (check-true (null? l)))

; Q: What is (null? (quote ())) ?
; A: True, because (quote ()) is a notation for the null list.
(check-true (null? '()))
(check-true (null? null))
(check-true (null? (quote ())))

; Q: is (null? l) true or false where l is (a b c) ?
; A: False, because l is a non-empty list.
(check-false (null? '(a b c)))

; Q: Is (null? a) true or false
; where a is spaghetti
; A: No answer, because you cannot ask null? of an atom.
; In practice, (null? x) is false for everything but the empty list.
(check-false (null? 'spaghetti))

; The Law of Null?
; The primitive null? is defined only for lists.

; Q: Is it true or false that s is an atom where s is Harry
; A: True, because Harry is a string of characters beginning with a letter.
(check-true (atom? 'Harry))

; Q: Is (atom? s) true or false where
; s is (Harry had a heap of apples)
; A: False, because s is a list.
(check-false (atom? '(Harry had a heap of apples)))

; Q: How many arguments does atom? take and what are they?
; A: 1 argument, any s-expression.

; Q: Is (atom? (car l)) true or false where
; l is (Harry had a heap of apples) ?
(let ((l '(Harry had a heap of apples)))
  (check-true (atom? (car l))))

; Q: Is (atom? (cdr l)) true or false where
; l is (Harry had a heap of apples)
; A: False, because (cdr l) is a list.
(let ((l '(Harry had a heap of apples)))
  (check-false (atom? (cdr l))))

; Q: is (atom? (cdr l)) true or false where
; l is (Harry)
; A: False, because () is not an atom -- () is a list.
(check-true (list? (cdr '(Harry))))
(check-false (atom? (cdr '(Harry))))

; Q: is (atom? (car (cdr l))) true or false
; where l is (swing low sweet cherry oat)
; A: True
(check-true (atom? (car (cdr '(swing low sweet cherry oat)))))

; Q: is (atom? (car (cdr l)) true or false where
; l is (swing (low sweet) cherry oat)
; A: False, because (cdr l) is ((low sweet) cherry oat)
; and the car of that is (low sweet), a list.
(check-false (atom? (car (cdr '(swing (low sweet) cherry oat)))))

; Q: True or false: a1 and a2 are the same atom
; where a1 is Harry and a2 is Harry
; A: True,
; because (eq? a1 a2) is just another way to ask
; "Are a1 and a2 the same non-numeric atom?
(let [(a1 'Harry) (a2 'Harry)]
  (check-true (eq? a1 a2)))

; Q: Is (eq? a1 a2) true or false
; where a1 is margarine and a2 is butter
; A: False, since a1 and a2 are different atoms.
(let [(a1 'margarine) (a2 'butter)]
  (check-false (eq? a1 a2)))

; Q: how many arguments does eq? take and what are they?
; A: 2 arguments, both must be non-numeric atoms.
; Real talk: in Racket, (eq? 2 2) -> #t
; and (eq? '(a) '(a)) -> #f, so go figure.

; Q: Is (eq? l1 l2) true or false where l1 is () and l2 is (strawberry)
; A: No answer, because the arguments are lists (not atoms).
; In reality, lists can be arguments to eq.
(let ((l1 '()) (l2 '(strawberry)))
  (check-false (eq? l1 l2)))

; Q: Is (eq? n1 n2) true or false
; where n1 is 6 an n2 is 7?
; A: No answer, but really the answer is #f.
(let ((n1 6) (n2 7))
  (check-false (eq? n1 n2)))
(check-true (eq? 2 2))
(check-true (eq? 1.0 1.0))
; note: not necessarily true that two floating point
; expressions that are mathematically equal would evaluate
; to the same internal representation....
(check-false (eq? (+ 1.0 0.5) (- 2.0 0.5)))
(check-true (equal? (+ 1.0 0.5) (- 2.0 0.5)))
; integers and floats are not equal? without coercion, because they
; are not the same type!
(check-false (equal? 1.0 1))
; integers and floats may be compared with =, however.
(check-true (= 1.0 1))

; Q: is (eq? (car l) a) true or false
; where l is (Mary had a little lamb chop)
; and a is Mary
; A: True, because (car l) and a are both the same
; atom, 'Mary
(let [(l '(Mary had a little lamb chop))
      (a 'Mary)]
  (check-true (eq? (car l) a)))

; Q: is (eq? (car l) (car (cdr l))) true or false
; where
; l is (beans beans we need jelly beans)
; A: True, because
; (car l) is 'beans,
; and because (cdr l) is (beans we need jelly beans), (car (cdr l)) is also 'beans.
(let ((l '(beans beans we need jelly beans)))
  (check-true (eq? (car l) (car (cdr l)))))

(print "This space reserved for JELLY STAINS!")