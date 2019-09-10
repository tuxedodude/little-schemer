#lang racket

(require rackunit)

; Chapter 3 of Little Schemer: Cons the Magnificent
; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else
       (cons
        (car lat)
        (rember a (cdr lat)))))))

; Q: What is rember a lat)
; where a is mint
; and lat is (lamb chops and mint jelly)
; A: (lamb chops and jelly)
; rember stands for "remove a member"
(let [(a 'mint)
      (lat '(lamb chops and jelly))]
  (check-equal?
   (rember a lat)
   '(lamb chops and jelly)))

; does nothing if no match
(let [(a 'toast)
      (lat '(bacon lettuce and tomato))]
  (check-equal? (rember a lat) '(bacon lettuce and tomato)))

; only removes the first match!
(let [(a 'cup)
      (lat '(coffee cup tea cup and hick cup))]
  (check-equal?
   (rember a lat)
   '(coffee tea cup and hick cup)))

; Q: What does (rember a lat) do?
; A: It takes and atom and a lat as arguments,
; and removes the first occurrence of the atom from the lat,
; leaving the lat alone if there is no match.

(println "The Second Commandment")
(println "Use cons to build lists")

; by the book
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons
        (car (car l))
        (firsts (cdr l)))))))

; succint
(define (firsts-2 l)
  (if (null? l) '()
      (cons (caar l) (firsts (cdr l)))))

; higher-order functions
(define (firsts-3 l)
  (map car l))

; Q: What is (firsts l) where
; l is ((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))
; A: (apple plum grape bean)

(define fruit
  '((apple peach pumpkin)
           (plum pear cherry)
           (grape raisin pea)
           (bean carrot eggplant)))

(let ([l '((apple peach pumpkin)
           (plum pear cherry)
           (grape raisin pea)
           (bean carrot eggplant))])
  (check-equal?
   (firsts l)
   '(apple plum grape bean))
  (check-equal?
   (firsts-2 l)
   '(apple plum grape bean))
  (check-equal?
   (firsts-3 l)
   '(apple plum grape bean)))

(define (seconds l)
  (map cadr l))

(check-equal? (seconds fruit) '(peach pear raisin carrot))

; Q: What is (insertR new old lat)
; where new is topping
; old is fudge
; and
; lat is (ice cream with fudge for dessert)
; A: (ice cream with fudge topping for dessert)
(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat))
       (cons old (cons new (cdr lat)))]
      [else
       (cons (car lat) (insertR new old (cdr lat)))])))

(let [(new 'topping)
      (old 'fudge)
      (lat '(ice cream with fudge for dessert))]
  (check-equal?
   (insertR new old lat)
   '(ice cream with fudge topping for dessert)))

; Q: insertR new old lat
; where new is jalapeno
; old is and
; and lat is (tacos tamales and salsa)
(check-equal?
 (insertR 'jalapeno 'and '(tacos tamales and salsa))
 '(tacos tamales and jalapeno salsa))

; Q: (insertR new old lat)
; where new is e
; old is d
; and lat is (a b c d f g d h)
; A: (a b c d e f g d h)
(check-equal?
 (insertR 'e 'd '(a b c d f g d h))
 '(a b c d e f g d h))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new lat))
      (else
       (cons
        (car lat)
        (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat))
       (cons new (cdr lat))]
      [else
       (cons (car lat)
             (subst new old (cdr lat)))])))

(define subst-2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? o1 (car lat))
           (eq? o2 (car lat)))
       (cons new (cdr lat))]
      [else
       (cons (car lat)
             (subst-2 new o1 o2 (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (multirember a (cdr lat))]
      [else
       (cons (car lat) (multirember a (cdr lat)))])))

(define (muuultirember a lat)
  (filter (negate (curry eq? a)) lat))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old (cons new
                       (multiinsertR new old (cdr lat))))]
      [else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))])))

(check-equal?
 (multiinsertR 'o 'r '(o r b o u r s))
 '(o r o b o u r o s))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old
                                 (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertL new old
                           (cdr lat)))])))

(check-equal?
 (multiinsertL 'o 'r '(r b r s))
 '(o r b o r s))

; higher order...
(define (subst-f match? modify lat)
  (define (subst-f lat)
    (let ((recurse (lambda () (subst-f (cdr lat)))))
      (cond
        [(null? lat) '()]
        [(match? (car lat))
         (modify (car lat) (recurse))]
        [else
         (cons (car lat)
               (recurse))])))
  (subst-f lat))

(define (multi-insert-R new old lat)
  (subst-f
   (curry eq? old)
   (lambda (a f)
       (cons a (cons new f)))
   lat))

(check-equal?
 (multi-insert-R 'old 'new '(new old older eldest new newest))
 '(new old old older eldest new old newest))

(define (translate table lat)
  (subst-f
   (lambda (term)
     (assoc term table))
   (lambda (term remaining)
       (cons (cdr (assoc term table))
             remaining)))
   lat)

(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat))
       (cons new
             (multisubst new old (cdr lat)))]
      [else
       (cons (car lat)
             (multisubst new old (cdr lat)))])))

(define (muultisubst new old lat)
  (map (lambda (x) (if (equal? x old) new x))
       lat))

(check-equal?
 (multisubst 'a 'x '(c d e a g x h x x x i l m x))
 '(c d e a g a h a a a i l m a))

(check-equal?
 (multisubst 'a 'x '())
 '())

(check-equal?
 (multisubst 'a 'x '(a))
 '(a))
