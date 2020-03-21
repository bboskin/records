#lang racket

(provide (all-defined-out))


;; struct definitions, basic functions.

(struct Record [title artist cost tracks facts])

(struct Store [grid mode bag])

;; basic list ops
(define (front-to-back ls)
  (if (null? ls) ls
      (append (cdr ls) (list (car ls)))))
(define (back-to-front ls)
  (if (null? ls) ls
      (let ((ls (reverse ls)))
        (cons (car ls) (reverse (cdr ls))))))