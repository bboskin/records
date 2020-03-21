#lang racket
(require "basics.rkt"
         "records.rkt"
         "drawing.rkt"
         "key-handling.rkt"
         2htdp/universe)

;; main file from which to run progam!

(define JAZZ (list HeavyWeather ALoveSupreme KindOfBlue PortraitInJazz Thrust))
(define HIPHOP (list Madvillainy TakeMeToYourLeader AnglesWithoutEdges AloneTogether))
(define TASKBAR1
  (list
   (list "Buy a Miles Davis album." #f)
   (list "Find an album sampled by another album." #f 50)
   (list "Find two records with Wayne Shorter." #f 30)
   (list "Buy five albums." #f)))

(define S1
  (Store
   `((#f (section ,HIPHOP) #f (clerk start))
     (#f #f #f #f)
     ((section ,JAZZ) #f #f #f)
     (#f #f #f player))
   'walking
   TASKBAR1
   '()
   50
   '()))

(big-bang S1
  [on-key key-handler]
  [to-draw draw])