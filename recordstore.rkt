#lang racket

(require "basics.rkt")

(provide find-player
         lookup-loc
         set-location
         set-player
         player-adjacent-to-records?
         player-adjacent-to-clerk?)
;; A file to implement the record store features


;; where is the player currently?
(define (find-player grid)
  (let loop ((x 0) (y 0) (grid grid))
    (match grid
      ['() (error "didn't find the player")]
      [`(() . ,rst) (loop 0 (add1 y) rst)]
      [`((,a . ,b) . ,rst) (if (eqv? a 'player) (values x y)
                               (loop (add1 x) y `(,b . ,rst)))])))

;; what is at the given location?
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

;; puts a given object at a given location (if that location has nothing there)
(define (set-location object grid x-loc y-loc)
  (let loop ((x 0) (y 0) (grid grid))
    (match grid
      ['() '()]
      [`(() . ,rst) (cons '() (loop 0 (add1 y) rst))]
      [`((,a . ,b) . ,rst)
       (let ((ans (loop (add1 x) y (cons b rst))))
         (cond
           [(and (eqv? x x-loc) (eqv? y y-loc) (not a))
            `((,object . ,(car ans)) . ,(cdr ans))]
           [else `((,a . ,(car ans)) . ,(cdr ans))]))])))

;; puts player at given location (and removes player from old location)
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

;; general way to ask if a player is near something (not provided)
(define (player-adjacent-to-object? symbol grid)
  (let-values (((x y) (find-player grid)))
    (let ((up (if (zero? y) #f `(,x ,(sub1 y))))
          (down (if (= y (sub1 (length grid))) #f `(,x ,(add1 y))))
          (left (if (zero? x) #f `(,(sub1 x) ,y)))
          (right (if (= x (sub1 (length (car grid)))) #f `(,(add1 x) ,y)))
          (test? (λ (x) (eqv? x symbol))))
      (foldr (λ (x a)
               (if (not x) a
                   (let ((val-at-x (lookup-loc grid (car x) (cadr x))))
                     (match val-at-x
                       [`(,(? test?) ,info) info]
                       [else a]))))
             #f
             (list up down left right)))))


;; wrapper function to ask about proximity
(define (player-adjacent-to-clerk? grid)
  (player-adjacent-to-object? 'clerk grid))
(define (player-adjacent-to-records? grid)
  (player-adjacent-to-object? 'section grid))
