#lang racket

(require "records.rkt"
         2htdp/image
         2htdp/universe)
;; A file to implement the record store features (including walking around and drawing everything)


;; Run this file to play! arrow keys to move around (you're the blue square), space bar to look at records or talk to clerk, x to go back to walking.

(struct Store (grid mode bag)
  #:transparent)

(define S1
  (Store
   '((#f (section ()) #f clerk)
     (#f #f #f #f)
     (#f #f #f #f)
     (#f #f #f player))
   'walking
   '()))

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
  (member 'clerk (player-is-adjacent-to grid)))
(define (player-adjacent-to-records? grid)
  (foldr
   (λ (x a)
     (match x
       [`(section ,recs) recs]
       [else a]))
   #f
   (player-is-adjacent-to grid)))

;; constants for drawing
(define SCENE-HEIGHT 200)
(define SCENE-WIDTH 200)
(define PLAYER-COLOR "blue")
(define CLERK-COLOR "red")
(define CLERK-DESK-COLOR "brown")
(define FLOOR-COLOR "tan")
(define (get-center-pixel-for-coord x k) (* k (+ x .5)))

;; drawing helpers for each kind of square

(define (draw-floor w h)
  (rectangle w h "solid" FLOOR-COLOR))
(define (draw-player w h)
  (overlay
   (circle (/ (min w h) 2) "solid" PLAYER-COLOR)
   (rectangle w h "solid" FLOOR-COLOR)))

(define (draw-clerk w h)
  (overlay
   (circle (/ (min w h) 2) "solid" CLERK-COLOR)
   (rectangle w h "solid" CLERK-DESK-COLOR)))

(define (draw-records w h)
  (if (zero? h) empty-image
      (above (rectangle w 1 "solid" (color (random 255) (random 255) (random 255)))
             (draw-records w (sub1 h)))))

;; drawing a column of the store
(define (draw-square s)
  (match s
    [#f draw-floor]
    ['clerk draw-clerk]
    ['player draw-player]
    [`(section ,records) draw-records]
    [else (error "Invalid object in the store")]))

(define (draw-store-background grid w h)
  (foldr
   (λ (r a) (above (foldr (λ (s a) (beside ((draw-square s) w h) a)) empty-image r) a))
   empty-image
   grid))

(define (draw-record-selection recs)
  (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white"))
(define (draw-clerk-interaction)
  (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white"))

(define (draw-record-store R)
  (match R
    [(Store grid mode bag)
     (let* ((k-cols (length grid))
            (k-rows (length (car grid)))
            (SPACE-WIDTH (/ SCENE-WIDTH k-cols))
            (SPACE-HEIGHT (/ SCENE-HEIGHT k-rows)))
       (let ((store-background (draw-store-background grid SPACE-WIDTH SPACE-HEIGHT)))
         (match mode
           ['walking store-background]
           [`(digging ,recs)
            (overlay (draw-record-selection recs)
                     store-background)]
           ['clerk
            (overlay (draw-clerk-interaction)
                     store-background)])))]))

;; handlers for different key inputs

(define (do-space grid bag)
  (cond
    [(player-adjacent-to-clerk? grid)
     (Store grid 'clerk bag)]
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
          [(string=? input "x")
           (Store grid 'walking bag)]
          [else R])]
       ['clerk
        (cond
          [(string=? input "x")
           (Store grid 'walking bag)]
          [else R])]
       [else R])]
    [else (error "not a store")]))




(big-bang S1
  [on-key key-handler]
  [to-draw draw-record-store])