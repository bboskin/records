#lang racket
(require "basics.rkt"
         "records.rkt"
         "drawing.rkt"
         "key-handling.rkt"
         2htdp/universe)

;; main file from which to run progam!

(define JAZZ (list ALoveSupreme KindOfBlue PortraitInJazz Thrust))
(define HIPHOP (list Madvillainy TakeMeToYourLeader AnglesWithoutEdges HeadnodSuite))
(define S1
  (Store
   `((#f (section ,HIPHOP) #f (clerk start))
     (#f #f #f #f)
     ((section ,JAZZ) #f #f #f)
     (#f #f #f player))
   'walking
   '()))

(big-bang S1
  [on-key key-handler]
  [to-draw draw-record-store])