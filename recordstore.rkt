#lang racket

(require "basics.rkt")

(provide (all-defined-out))
;; A file to implement the record store features


;; questions/probes we can ask about a grid (and the locations of a player)
(define (find-player grid)
  (let loop ((x 0) (y 0) (grid grid))
    (match grid
      ['() (error "didn't find the player")]
      [`(() . ,rst) (loop 0 (add1 y) rst)]
      [`((,a . ,b) . ,rst) (if (eqv? a 'player) (values x y)
                               (loop (add1 x) y `(,b . ,rst)))])))
(define (lookup-loc grid x-loc y-loc)
  (let loop ((x x-loc) (y y-loc) (grid grid))
    (match grid
      ['() (error (format "invalid location to lookup ~s ~s" x-loc y-loc))]
      [`(() . ,rst) (error "invalid location to lookupvv ~s ~s" x-loc y-loc)]
      [`((,a . ,b) . ,rst)
       (if (zero? y)
           (if (zero? x) a
               (loop (sub1 x) 0 (cons b rst)))
           (loop x (sub1 y) rst))])))
(define (set-player grid x-loc y-loc)
  (let loop ((x 0) (y 0) (grid grid))
    (match grid
      ['() '()]
      [`(() . ,rst) (cons '() (loop 0 (add1 y) rst))]
      [`((,a . ,b) . ,rst)
       (let ((ans (loop (add1 x) y (cons b rst))))
         (cond
           [(and (eqv? x x-loc) (eqv? y y-loc))
            `((player . ,(car ans)) . ,(cdr ans))]
           [(eqv? a 'player)
            `((#f . ,(car ans)) . ,(cdr ans))]
           [else `((,a . ,(car ans)) . ,(cdr ans))]))])))

(define (player-is-adjacent-to grid)
  (let-values (((x y) (find-player grid)))
    (let ((up (if (zero? y) #f `(,x ,(sub1 y))))
          (down (if (= y (sub1 (length grid))) #f `(,x ,(add1 y))))
          (left (if (zero? x) #f `(,(sub1 x) ,y)))
          (right (if (= x (sub1 (length (car grid)))) #f `(,(add1 x) ,y))))
      (map
       (λ (x) (lookup-loc grid (car x) (cadr x)))
       (filter (λ (x) x) (list up down left right))))))

(define (player-adjacent-to-clerk? grid)
  (foldr
   (λ (x a)
     (match x
       [`(clerk ,mode) mode]
       [else a]))
   #f
   (player-is-adjacent-to grid)))
(define (player-adjacent-to-records? grid)
  (foldr
   (λ (x a)
     (match x
       [`(section ,recs) recs]
       [else a]))
   #f
   (player-is-adjacent-to grid)))
