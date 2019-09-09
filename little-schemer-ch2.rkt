#lang racket

(require rackunit)

; Chapter 2 of Little Schemer.
; Answers to the Socratic dialogue are given
; mostly in the form of unit test assertions, with
; the question/answer text commented above the assertion.

(define (atom? x)
  ; check if argument is an atom
 (nor (pair? x) (null? x)))