#lang racket

(require 2htdp/image
         "basics.rkt"
         "tasks.rkt")

(provide draw)

;; code for drawing record stores.



;; constants for drawing
(define SCENE-HEIGHT 500)
(define SCENE-WIDTH 500)
(define PLAYER-COLOR "blue")
(define CLERK-COLOR "red")
(define CLERK-DESK-COLOR "brown")
(define CLERK-TEXT-COLOR "brown")
(define FLOOR-COLOR "tan")
(define RECORD-TEXT-SIZE 20)
(define RECORD-TEXT-COLOR "blue")
(define NO-RECORD-TEXT-COLOR "red")
(define NO-RECORDS-MESSAGE "No Records Here...")

;; drawing helpers for each kind of square (to be improved)

(define (draw-player w h)
  (let ()
    (overlay
     (above
     (circle (floor (/ (- h (/ h 5) (/ h 3)) 3)) "solid" "beige")
     (beside
      (rectangle (/ w 10) (/ h 3) "solid" "red")
      (overlay
       (square (/ h 5) "solid" "red")
       (rectangle (/ w 3) (/ h 3) "solid" "white"))
      (rectangle (/ w 10) (/ h 3) "solid" "red"))
     (rectangle (/ w 3) (/ h 3) "solid" "brown")
     )
     (rectangle w h "solid" FLOOR-COLOR))))

#;
(define (draw-player w h)
  (overlay
   (circle (/ (min w h) 2) "solid" PLAYER-COLOR)
   (rectangle w h "solid" FLOOR-COLOR)))

(define (draw-floor w h)
  (rectangle w h "solid" FLOOR-COLOR))

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
            (text (string-append "" (number->string cost)) RECORD-TEXT-SIZE RECORD-TEXT-COLOR))]))
(define (draw-record-selection recs)
  (overlay
   (if (null? recs) (text NO-RECORDS-MESSAGE RECORD-TEXT-SIZE NO-RECORD-TEXT-COLOR) (draw-record (car recs)))
   (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white")))


(define (clerk-text s)
  (text s RECORD-TEXT-SIZE CLERK-TEXT-COLOR))

(define (draw-clerk-interaction mode)
  (overlay
   (match mode
    ['start
     (above
       (clerk-text "what can i do ya for?")
       (clerk-text "press b to buy the current records in your bag.")
       (clerk-text "press t if you finished a task!")
       (clerk-text "press r to restart our conversation."))]
     ['bought
      (clerk-text "great choices!")]
     ['not-enough
      (clerk-text "you need more money!")]
     ['task-done
      (clerk-text "Way to go!")]
     ['no-task
      (clerk-text "Sorry, that didn't complete any new tasks")])
   (rectangle (* SCENE-WIDTH 4/5) (* SCENE-HEIGHT 4/5) "solid" "white")))

(define (draw-record-store R)
  (match R
    [(Store grid mode taskbar bag money owned)
     (let* ((k-cols (length grid))
            (k-rows (length (car grid)))
            (SPACE-WIDTH (/ SCENE-WIDTH k-cols))
            (SPACE-HEIGHT (/ SCENE-HEIGHT k-rows)))
       (let ((store-background (draw-store-background grid SPACE-WIDTH SPACE-HEIGHT)))
         (match mode
           ['walking store-background]
           ['won store-background]
           [`(digging ,recs)
            (overlay (draw-record-selection recs)
                     store-background)]
           [`(clerk ,mode)
            (overlay (draw-clerk-interaction mode)
                     store-background)])))]))


;;; a sidebar with useful info
(define SIDEBAR-TEXT-COLOR "black")
(define SIDEBAR-TEXT-SIZE 20)
(define SIDEBAR-BACKGROUND-COLOR "white")
(define (sidebar-text x) (text x SIDEBAR-TEXT-SIZE SIDEBAR-TEXT-COLOR))
(define (draw-sidebar R)
  (match R
    [(Store grid mode taskbar bag money owned)
     (overlay
      (above (sidebar-text (format "Cash: $~s" money))
             (sidebar-text "\n\n")
             (sidebar-text (format "Record Bag:"))
             (foldr above empty-image (map (λ (x) (sidebar-text (string-append (Record-title x) ": $" (number->string (Record-cost x))))) bag))
             (sidebar-text "\n\n")
             (sidebar-text "To walk around,")
             (sidebar-text "use the arrow keys.")
             (sidebar-text "To look at records,")
             (sidebar-text "walk up to a shelf and press space.")
             (sidebar-text "To put a record in your bag,")
             (sidebar-text "press space while looking at the one you want.")
             (sidebar-text "To talk to the clerk,")
             (sidebar-text "walk up to him and press space.")
             (sidebar-text "To return to the floor from a submenu,")
             (sidebar-text "press x.")
             (sidebar-text "To empty your record bag, press d."))
      (rectangle (/ SCENE-WIDTH 2) SCENE-HEIGHT "solid" SIDEBAR-BACKGROUND-COLOR))]))

(define (draw-personal-collection R)
  (match R
    [(Store grid mode taskbar bag money owned)
     (overlay
      (foldr
       (λ (x a) (above (text (Record-title x) 20 "black") a))
       empty-image
      owned)
      (rectangle (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 2) "solid" SIDEBAR-BACKGROUND-COLOR))]))

;; drawing the taskbar
(define TASKBAR-BACKGROUND-COLOR "white")
(define TASKBAR-TEXT-SIZE 20)
(define TASKBAR-DONE-TEXT-COLOR "green")
(define TASKBAR-NOT-DONE-TEXT-COLOR "red")
(define TASKBAR-PAYMENT-COLOR "orange")
(define (draw-task t)
  (match t
    [(Task tt reward test?)
     (above
      (text tt TASKBAR-TEXT-SIZE TASKBAR-NOT-DONE-TEXT-COLOR)
      (text (format "Pays: $~s and ~s records" (car reward) (length (cdr reward))) TASKBAR-TEXT-SIZE TASKBAR-PAYMENT-COLOR))]))

(define (draw-taskbar R)
  (match R
    [(Store grid mode taskbar bag money owned)
     (overlay
      (if (null? taskbar)
          (text "You beat the level!" TASKBAR-TEXT-SIZE TASKBAR-PAYMENT-COLOR)
          (foldr
           (λ (x a) (above (draw-task x) a))
           empty-image
           taskbar))
      (rectangle (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 2) "solid" TASKBAR-BACKGROUND-COLOR))]))

;; wrapper
(define (draw R)
  (beside
   (draw-record-store R)
   (draw-sidebar R)
   (above
    (draw-taskbar R)
    (draw-personal-collection R))))