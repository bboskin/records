#lang racket

;; all functions related to key-handling

(require "basics.rkt"
         "recordstore.rkt")
(provide key-handler)

;; handlers for different key inputs

(define (do-space grid bag)
  (cond
    [(player-adjacent-to-clerk? grid)
     =>
     (λ (mode) (Store grid `(clerk ,mode) bag))]
    [(player-adjacent-to-records? grid)
     =>
     (λ (recs) (Store grid `(digging ,recs) bag))]
    [else (Store grid 'walking bag)]))

(define (do-up grid)
  (let-values (((x y) (find-player grid)))
    (cond
      [(zero? y) grid]
      [(not (list-ref (list-ref grid (sub1 y)) x))
       (set-player grid x (sub1 y))]
      [else grid])))

(define (do-down grid)
  (let-values (((x y) (find-player grid)))
    (cond
      [(= y (sub1 (length grid))) grid]
      [(not (list-ref (list-ref grid (add1 y)) x))
       (set-player grid x (add1 y))]
      [else grid])))
(define (do-left grid)
  (let-values (((x y) (find-player grid)))
    (cond
      [(zero? x) grid]
      [(not (list-ref (list-ref grid y) (sub1 x)))
       (set-player grid (sub1 x) y)]
      [else grid])))
(define (do-right grid)
  (let-values (((x y) (find-player grid)))
    (cond
      [(= x (sub1 (length (car grid)))) grid]
      [(not (list-ref (list-ref grid y) (add1 x)))
       (set-player grid (add1 x) y)]
      [else grid])))

;; key-handler
(define (key-handler R input)
  (match R
    [(Store grid mode bag)
     (match mode
       ['walking
        (cond
          [(string=? input " ") (do-space grid bag)]
          [(string=? input "up") (Store (do-up grid) 'walking bag)]
          [(string=? input "down") (Store (do-down grid) 'walking bag)]
          [(string=? input "left") (Store (do-left grid) 'walking bag)]
          [(string=? input "right") (Store (do-right grid) 'walking bag)]
          [else R])]
       [`(digging ,recs)
        (cond
          [(string=? input "x") (Store grid 'walking bag)]
          [(string=? input "up") (Store grid `(digging ,(front-to-back recs)) bag)]
          [(string=? input "down") (Store grid `(digging ,(back-to-front recs)) bag)]
          [(string=? input " ") (if (null? recs) R (Store grid `(digging ,(cdr recs)) (cons (car recs) bag)))]
          [else R])]
       [`(clerk ,mode)
        (cond
          [(string=? input "x") (Store grid 'walking bag)]
          [else R])]
       [else R])]
    [else (error "not a store")]))