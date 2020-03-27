#lang racket

(provide (all-defined-out))


;; struct definitions

(struct Record [title artist cost tracks facts])

(struct Store [grid mode taskbar record-bag money owned])

;; basic list ops
(define (front-to-back ls)
  (if (null? ls) ls
      (append (cdr ls) (list (car ls)))))

(define (back-to-front ls)
  (if (null? ls) ls
      (let ((ls (reverse ls)))
        (cons (car ls) (reverse (cdr ls))))))
