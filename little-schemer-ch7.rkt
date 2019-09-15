#lang racket

(require rackunit)

; Chapter 7 of Little Schemer: Friends and relations

; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

(define (member? a lat)
  (if (null? lat) #f
      (if (eq? a (car lat)) #t
          (member? a (cdr lat)))))

(check-false (member? 'a '()))
(check-true (member? 'a '(a)))
(check-false (member? 'a '(b)))
(check-false (member? 'a '(b c d)))

(define (set? lat)
  (if (null? lat) #t
      (and (not (member? (car lat) (cdr lat)))
           (set? (cdr lat)))))

(check-true (set? '()))
(check-true (set? '(1)))
(check-true (set? '(())))
(check-true (set? '(1 2 3)))
(check-false (set? '(1 2 3 1)))

(define (makeset lat)
  (if (null? lat) '()
      (if (member? (car lat) (cdr lat))
          (makeset (cdr lat))
          (cons (car lat) (makeset (cdr lat))))))

(define (subset? s1 s2)
  ; is s1 a subset of s2?
  (or (null? s1) 
      (and (member? (car s1) s2)
           (subset? (cdr s1) s2))))

(check-true (subset? '() '(1 2 3 4)))
(check-true (subset? '(1) '(1 2 3 4)))
(check-true (subset? '(1 3) '(1 2 3 4)))
(check-false (subset? '(1) '()))
(check-false (subset? '(1 9) '(1 2 3 4)))
(check-true (subset? '() '()))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(check-true (eqset? '() '()))
(check-true (eqset? '(1 2 3) '(3 1 2)))
(check-false (eqset? '(1 2 4) '(1 2 5)))

(check-equal? (makeset '(apple peach pear peach plum apple lemon peach))
                       '(pear plum apple lemon peach))
(check-true (eqset? (makeset '(a b c a d g)) '(a b c d g)))

(define (intersect? s1 s2)
  (if (null? s1) #f
      (or (member? (car s1) s2)
          (intersect? (cdr s1) s2))))

(define (intersect s1 s2)
  (define (intersect s)
    (if (null? s) '()
        (if (member? (car s) s2)
            (cons (car s) (intersect (cdr s)))
            (intersect (cdr s)))))
  (intersect s1))

(define (multirember a l)
  (define (MR l)
    (if (null? l) '()
        (if (atom? (car l))
            (if (equal? a (car l))
                (MR (cdr l))
                (cons (car l) (MR (cdr l))))
            (cons (MR (car l)) (MR (cdr l))))))
  (MR l))

(define (make-set lat)
  (if (null? lat) '()
      (let [(this (car lat))
            (that (cdr lat))]
        (cons this
              (make-set
               (multirember this that))))))

(define (union s1 s2)
  (make-set (append s1 s2)))

(define (set-difference s1 s2)
  (let ((recur (lambda ()
                 (set-difference (cdr s1) s2))))
    (cond
      ((null? s1) '())
      ((member? (car s1) s2) (recur))
      (else
       (cons (car s1)
             (recur))))))

(define (intersect-all l-set)
  (cond
    [(null? l-set) '()]
    [(null? (cdr l-set)) (car l-set)]
    [else
      (intersect (car l-set)
                 (intersect-all (cdr l-set)))]))
 
(define (a-pair? x)
  (cond
    [(null? x) #f]
    [(atom? x) #f]
    [(null? (cdr x)) #f]
    [(null? (cddr x)) #t]))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define build list)

; relation, that is a list of pairs
(define (rel? lst)
  (andmap a-pair? lst))

(define (firsts rel)
  (map 1st rel))

; mathematical function, that is a relation
; of pairs (x y) in which x does not occur more than
; once
(define (function? rel)
  (set? (firsts rel)))

(define (revpair p)
  (list (second p) (first p)))

(define (revrel rel)
  (map revpair rel))

(define (one-to-one? fun)
  (function? (revrel fun)))

;---------------------------------
; sorting

; given the current element,
;

(define (eqlist? a b)
  (cond
    [(and (null? a) (null? b)) #t]
    [(or (null? a) (null? b)) #f]
    [else
     (and (equal? (car a) (car b))
          (eqlist? (cdr a) (cdr b)))]))

(check-true (eqlist? '(a b c) '(a b c)))
(check-false (eqlist? '(a b c) '(a b c d)))
(check-true (eqlist? '() '()))

(define (bubble lst)
  (printf "(bubble ~a)~n" lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) lst]
    ;must have 2 elements
    [else
     (let ((A (car lst))
           (B (cadr lst))
           (C (cddr lst)))
     (if (>= A B)
         (cons B (bubble (cons A C)))
         (cons A (bubble (cons B C)))))]))


(define (bubblesort lst)
  (let ((result (bubble lst)))
      (if (eqlist? result lst) result
          (bubblesort result))))


