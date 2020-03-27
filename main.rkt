#lang racket
(require "basics.rkt"
         "records.rkt"
         "drawing.rkt"
         "key-handling.rkt"
         "tasks.rkt"
         2htdp/universe)

;; main file from which to run progam!

(define JAZZ (list HeavyWeather ALoveSupreme KindOfBlue PortraitInJazz Thrust))
(define HIPHOP (list Madvillainy TakeMeToYourLeader AnglesWithoutEdges AloneTogether))
(define SNUG  (list FreckleSeason NoDogsAllowed Human FineLine))
(define TASKBAR1
  (list
   (Task "Buy a Miles Davis album."
         '(0)
         (λ (b o)
           (ormap (λ (x) (string=? "Miles Davis" (Record-artist x))) o)))
   (Task "Find an album sampled by another album."
         '(40)
         (λ (b o)
           (ormap (λ (r) (sampled-by-any? r (append b o)))
                  (append b o))))
   (Task "Find two records with Wayne Shorter." '(30)
         (λ (b o)
           (ormap (λ (r) (or (member '(wayne-shorter plays alto-saxophone) (Record-facts r))
                             (string=? "Wayne Shorter" (Record-artist r))))
                  (append b o))))
   (Task "Buy five albums." '(0)
         (λ (b o) (>= (length o) 5)))))

(define S1
  (Store
   `((#f (section ,HIPHOP) #f #f)
     ((clerk start) #f #f #f)
     (#f #f #f (section ,JAZZ))
     ((section ,SNUG) #f #f player))
   'walking
   TASKBAR1
   '()
   50
   '()))

(big-bang S1
  [on-key key-handler]
  [to-draw draw])


#|
TODO:
implement task-checking, ability to get money
make the records only change color when you're next to them (and move record menu somewhere else)

|#