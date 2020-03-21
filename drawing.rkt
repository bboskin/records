#lang racket

(require 2htdp/image
         "basics.rkt")

(provide draw-record-store)

;; code for drawing record stores.



;; constants for drawing
(define SCENE-HEIGHT 200)
(define SCENE-WIDTH 200)
(define PLAYER-COLOR "blue")
(define CLERK-COLOR "red")
(define CLERK-DESK-COLOR "brown")
(define CLERK-TEXT-COLOR "brown")
(define FLOOR-COLOR "tan")
(define RECORD-TEXT-SIZE 12)
(define RECORD-TEXT-COLOR "blue")
(define NO-RECORD-TEXT-COLOR "red")
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
    [`(clerk ,mode) draw-clerk]
    ['player draw-player]
    [`(section ,records) draw-records]
    [else (error "Invalid object in the store")]))

(define (draw-store-background grid w h)
  (foldr
   (λ (r a) (above (foldr (λ (s a) (beside ((draw-square s) w h) a)) empty-image r) a))
   empty-image
   grid))

(define (draw-record r)
  (match r
    [(Record title artist cost tracks facts)
     (above (text title RECORD-TEXT-SIZE RECORD-TEXT-COLOR)
            (text artist RECORD-TEXT-SIZE RECORD-TEXT-COLOR)
            (text cost RECORD-TEXT-SIZE RECORD-TEXT-COLOR))]))
(define (draw-record-selection recs)
  (overlay
   (if (null? recs) (text "No Records Here..." RECORD-TEXT-SIZE NO-RECORD-TEXT-COLOR) (draw-record (car recs)))
   (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white")))
(define (draw-clerk-interaction mode)
  (overlay
   (text "what can i do ya for?" RECORD-TEXT-SIZE CLERK-TEXT-COLOR)
   (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white")))

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
           [`(clerk ,mode)
            (overlay (draw-clerk-interaction mode)
                     store-background)])))]))