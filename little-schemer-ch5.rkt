#lang racket


(require rackunit)

; Chapter 5 of Little Schemer: It's full of stars

; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))

(define (andf a? b? arg)
  (and (a? arg) (b? arg)))

(define (rember* a l)
  (define (rember* l)
    (if (null? l) '()
        (let* ((A (car l))
               (B (cdr l))
               (R (lambda () rember* B)))
          (if (atom? A)
              (if (eq? a A)
                  (rember* B)
                  (cons A (R)))
              (cons (rember* A)
                    (rember* (R)))))))
  (rember* l))

(let [(a 'cup)
      (l '((coffee) cup ((tea) cup) (and (hick)) cup))]
  (check-equal?
   (rember* a l)
   '((coffee) ((tea)) (and (hick)))))

(define (lat? l)
  (not (null? (filter atom? l))))

(define (insertR* new old l)
  (define (F l)
    (if (null? l) '()
        (if (atom? (car l))
            (if (eq? old (car l))
                (cons old (cons new (F (cdr l))))
                (cons (car l) (F (cdr l))))
            (cons (F (car l))
                  (F (cdr l))))))
    (F l))

(let ([new 'roast]
      [old 'chuck]
      [l '((how much (wood))
           could
           ((a (wood) chuck))
           (((chuck)))
           (if (a) ((wood chuck)))
           could chuck wood)])
  (check-equal?
   (insertR* new old l)
   '(( how much (wood))
     could
     ((a (wood) chuck roast))
     (((chuck roast)))
     (if (a) ((wood chuck roast)))
     could chuck roast wood)))

(define occur*
  (lambda (a l)
    (if (null? l) 0
        (if (atom? (car l))
            (if (eq? a (car l))
                (+ 1 (occur* a (cdr l)))
                (occur* a (cdr l)))
            (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(check-equal? (occur* 'a '((((a)) a a) b ((c d a) a))) 5)

(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (eq? (car l) old)
           (cons new (subst* new old (cdr l)))
           (cons (car l) (subst* new old (cdr l))))]
      [else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))])))

(check-equal? (subst* 'new 'old '(old (((old) a b) x old) old))
              '(new (((new) a b) x new) new))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       [if (eq? (car l) old)
           (cons new (cons old (insertL* new old (cdr l))))
           (cons (car l) (insertL* new old (cdr l)))]]
      [else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))])))

(check-equal? (insertL* 'b 'a '(a (silly 9 fox) a ((((a jumped))))))
              '(b a (silly 9 fox) b a ((((b a jumped))))))

(define member*
  (lambda (a l)
    (if (null? l) '()
        (if (atom? (car l))
            (if (eq? a (car l))
                #t
                (member* a (cdr l)))
            (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (if (null? l) '() ; book implementation assumes no empty list
        (if (atom? (car l))
            (car l)
            (leftmost (car l))))))

(check-equal?
 (leftmost '((potato (chips ((with) fish) (chips)))))
 'potato)

(check-equal?
 (leftmost '(((hot) (tuna (and))) cheese))
 'hot)

(define (both? q? x y)
  (and (q? x) (q? y)))

(define (xor? q? x y)
  (xor (q? x) (q? y)))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(xor? null? l1 l2) #f]
      [(both? null? l1 l2) #t]
      [(xor? atom? l1 l2) #f]
      [(both? atom? l1 l2) (eq? l1 l2)]
      (else
       (let* ([A (car l1)]
              (AA (cdr l1))
              (B (car l2))
              (BB (cdr l2))
              (recar? (lambda () (eqlist? A B)))
              (recdr? (lambda () (eqlist? AA BB))))
        
         (cond
           [(xor? atom? A B) #f]
           [(both? atom? A B) (recdr?)]
           [else
            (and (recar?)
                 (recdr?))]))))))
           

(check-true (eqlist? '(a b c) '(a b c)))
(check-false (eqlist? '(a b c) '()))
(check-false (eqlist? '(a b c d) '(a b c)))
(check-true (eqlist? '((a b) c) '((a b) c)))

