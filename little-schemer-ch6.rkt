#lang racket

(require rackunit)

; Chapter 6 of Little Schemer: Shadows

; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

(check-equal? (quote a) 'a)
(check-equal? (quote +) '+)
(let ((y 'a))
  (check-true (equal? (quote a) y)))
; --------------------------------------
; infix expressions
(define operators '(+ * ^))

(define (operator? x)
  (ormap (curry eq? x) operators))

(check-true (operator? '+))
(check-true (operator? '*))
(check-true (operator? '^))
(check-false (operator? 'a))

(define op cadr)
(define first car)
(define second caddr)
(define (singleton? s) (null? (cdr s)))

(define (numbered? expr)
  (if (atom? expr) (number? expr)
      (if (singleton? expr)
          (numbered? (car expr))
          (and (numbered? (first expr))
               (numbered? (second expr))))))

(check-true (numbered? '1))
(check-true (numbered? '(1 + 1)))
(check-true (numbered? '(1 + (2 * 3))))
(check-false (numbered? '(2 * sausage)))
(check-true (numbered? '(1)))
(check-true (numbered? '(((((((1)))))))))

(define (member? a lat)
  (and (not (null? lat))
       (or (eq? a (car lat))
           (member? a (cdr lat)))))

(define optable
  `((* . ,(lambda (x y) (* (value x) (value y))))
    (+ . ,(lambda (x y) (+ (value x) (value y))))
    (^ . ,(lambda (x y) (expt (value x) (value y))))))

(define (apply-op op a b)
  ((cdr (assoc op optable)) a b))

(define (value s)
  (cond
    [(atom? s) s]
    [(singleton? s) (value (car s))]
    [(operator? (op s))
     (apply-op (op s)
               (first s)
               (second s))]))

(check-equal? (value 1) 1)
(check-equal? (value '(1)) 1)
(check-equal? (value '(1 + 1)) 2)
(check-equal? (value '((1) + 1)) 2)
(check-equal? (value '((1) + (1))) 2)
(check-equal? (value '((2 * 3) + (4 * 5))) 26)
(check-equal? (value '(((1 + 1) * 3) + ((2 ^ 2) * 5))) 26)

; null-list encoding
(define sero? null?)

(check-true (sero? '()))

(define (add1 n)
  (cons '() n))

(check-equal? (add1 '()) '(()))
(check-equal? (add1 '(())) '(() ()))
(check-equal? (add1 '(() ())) '(() () ()))

(define sub1 cdr)

(define (subtract a b)
  (if (or (sero? b) (sero? a)) a
      (subtract (sub1 a) (sub1 b))))

(define (add a b)
  (if (sero? b) a
      (add (add1 a) (sub1 b))))

(define (gte a b)
  (if (and (sero? a) (not (sero? b))) #f
      (or (and (sero? a)) (sero? b)
          (gte (sub1 a) (sub1 b)))))

(define (null-encode n)
  (if (= 0 n) '()
      (cons '() (null-encode (- n 1)))))

(define (null-decode n)
  (if (sero? n) 0
      (+ 1 (null-decode (sub1 n)))))

(check-equal? (null-encode (null-decode '(() ()))) '(() ()))
(check-equal? (null-decode (null-encode 10)) 10)

; this is a bit weird with improper divisors.
(define (divide a b)
  (if (sero? a) '()
      (if (gte a b)
          (add1 (divide (subtract a b) b))
          '())))

(check-equal? (divide (null-encode 10) (null-encode 5)) (null-encode 2))
(check-equal? (divide (null-encode 10) (null-encode 3)) (null-encode 3))

(define nil '())

;(define (gr ops s)
;  (define (gr s)
;    (println s)
;    (if (null? s) '()
;        (if (null? (cdr s)) s
;            (let ((o (op s)))
;              (if (member? o ops)
;                  (let ((me (list (car s) o (caddr s))))
;                    (println "member")
;                    (println me)
;                    (gr (cons me (cdddr s))))
;                  (begin
;                    (println "not member")
;                    (cons (car s)
;                          (cons o (gr (cddr s))))))))))
;  (gr s))

(define (take n lst)
  (if (zero? n) '()
      (cons (car lst)
            (take (- n 1) (cdr lst)))))

(define (dyad? s)
  (and (not (null? s))
       (not (null? (cdr s)))
       (null? (cdddr s))))


(define (group ops s)
  (define (G s)
    (cond
      [(null? s) '()]
      [(singleton? s) s]
      [(member? (op s) ops)
       (let [(grouping (take 3 s))]
         (G (cons grouping (cdddr s))))]
      [else
       (cons (car s) (cons (op s) (G (cddr s))))]))
  (G s))
  

(define precedence-table
  '((^)
    (*)
    (+ -)))

(define (group-by-precedence s)
  (define (G table s)
    (if (null? table) s
        (G (cdr table) (group (car table) s))))
  (G precedence-table s))
          

(define a '(1 + 2 * 3))
(define b '(0 + 1 + (2 * 3)))
(define c '((((1 + 2) + ((3 * 5) * (9 + 1))) + 2)))

(check-equal? (group nil a) a)
(check-equal? (group '(*) a) '(1 + (2 * 3)))

; infix not finished
(define (infix s)
    (define (V e)
      (cond
        [(null? e) '()]
        [(atom? e) e]
        [(singleton? e) (infix (car e))]
        [else
         (let ((E (group-by-precedence e)))
           (if (singleton? E) (infix (car E))
               ; must be list of at least 3 cells
               (let [(o (op E))
                     (L (car E))
                     (R (cddr E))]
                 (cond
                   [(eq? o '+) (+ (infix L) (infix R))]
                   [(eq? o '-) (- (infix L) (infix R))]
                   [(eq? o '*) (* (infix L) (infix R))]
                   [(eq? o '^) (expt (infix L) (infix R))]
                   [else
                    'bad-expression]))))]))
  (V s))

(check-equal? (infix 1) 1)
(check-equal? (infix '()) '())
(check-equal? (infix '(1)) 1)