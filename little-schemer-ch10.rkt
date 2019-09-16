#lang racket

(require rackunit)

(define (atom? x)
  (nor (pair? x) (null? x)))


; like acons in lisp (a-list cons)
; adds entry of (key . value) pair to the association list (table)
(define (bind key val table)
  (cons (cons key val) table))

(define extend-table list)

(define (lookup key env)
  (if (null? env) '()
      (let ((val (assoc key (car env))))
        (if val (cdr val)
            ; next oldest environment
            (lookup key (cdr env))))))



(define a (bind 'a 10 '()))
(define b (bind 'b 20 '()))
(define c (bind 'c 30 '()))

(define e (list a b c))

(define consts
  '(#t #f 'cons 'car 'cdr 'null?
       'eq? atom? zero? add1 sub1 number? nil))

; return the list, not the first argument,
; as per picolisp
(define text-of cdr)

(define (*quote e table)
  (text-of e))

(define (*const e table)
  (cond
    [(number? e) e]
    [(eq? e #t) #t]
    [(eq? e #f) #f]
    [else
     (list 'primitive e)]))

(define (*identifier e table)
  (lookup e table))

(define (atom-to-action e)
  (if
   (or
      (number? e)
      (ormap (curry eq? e) consts))
   *const
   *identifier))

; ----------------------------
(define (*lambda e table)
  (list 'non-primitive
        (cons table (cdr e))))

(define table-of car)
(define formals-of cadr)
(define body-of caddr)

(define (evcond lines table)
  ; evaluate a cond expression composed
  ; of cond lines: either (test result)
  ; or (else? result)
  (define (else? x)
    (and (atom? x) (eq? x ('else))))

  (define test car)
  (define result cadr)
  
  (cond
    [(else? (caar lines))
     (meaning (cadar lines) table)]
    [(meaning (caar lines) table)
     (meaning (cadar lines) table)]
    [else
     (evcond (cdr lines) table)]))

(let [(cond-lines
       '([(eq? a b) c]
         [(null? d) (do-something x)]
         [else? 'give-me-cookies]))]
  (
     

;------------------------------
  
(define *cond '())
(define *application '())

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (list-to-action e)
  (if
   (atom? (car e))
          (let ((e? (curry eq? (car e))))
            (cond
              [(e? 'quote) *quote]
              [(e? 'lambda) *lambda]
              [(e? 'cond) *cond]
              [else
               *application]))
          ;else
          *application))

(define (expression-to-action e)
  (if (atom? e) (atom-to-action e)
      (list-to-action e)))