#lang racket

;; all functions related to key-handling

(require "basics.rkt"
         "recordstore.rkt"
         "tasks.rkt")
(provide key-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; handlers used in WALKING mode

(define (do-space grid taskbar bag money owned)
  (cond
    [(player-adjacent-to-clerk? grid)
     =>
     (λ (mode) (Store grid `(clerk ,mode) taskbar bag money owned))]
    [(player-adjacent-to-records? grid)
     =>
     (λ (recs) (Store grid `(digging ,recs) taskbar bag money owned))]
    [else (Store grid 'walking taskbar bag money owned)]))

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

(define (walk R input)
  (match R
    [(Store grid mode taskbar bag money owned)
     (match input
          [" " (do-space grid taskbar bag money owned)]
          ["d" (Store grid 'walking taskbar '() money owned)]
          ["up" (Store (do-up grid) 'walking taskbar bag money owned)]
          ["down" (Store (do-down grid) 'walking taskbar bag money owned)]
          ["left" (Store (do-left grid) 'walking taskbar bag money owned)]
          ["right" (Store (do-right grid) 'walking taskbar bag money owned)]
          [else R])]))

;; key-handler
(define (key-handler R input)
  (match R
    [(Store grid mode taskbar bag money owned)
     (match mode
       ['walking (walk R input)]
       ['won (walk R input)]
       [`(digging ,recs)
        (match input
          ["x" (Store grid 'walking taskbar bag money owned)]
          ["up" (Store grid `(digging ,(front-to-back recs)) taskbar bag money owned)]
          ["down" (Store grid `(digging ,(back-to-front recs)) taskbar bag money owned)]
          [" " (if (null? recs) R (Store grid `(digging ,(cdr recs)) taskbar (cons (car recs) bag) money owned))]
          [else R])]
       [`(clerk ,mode)
        (match input
          ["x" (Store grid 'walking taskbar bag money owned)]
          [else (match mode
                  ['start
                   (match input
                     ["b" (let ((cost-of-bag (foldr (λ (x a) (+ (Record-cost x) a)) 0 bag)))
                            (if (>= money cost-of-bag)
                              (Store grid '(clerk bought) taskbar '() (- money cost-of-bag) (append bag owned))
                              (Store grid '(clerk not-enough) taskbar bag money owned)))]
                     ["t" (let-values (((money-reward record-reward new-taskbar) (check-tasks taskbar bag owned)))
                            (if (= (length taskbar) (length new-taskbar))
                                (Store grid '(clerk no-task) new-taskbar '() (+ money-reward money) (append record-reward owned))
                                (if (null? taskbar)
                                    (Store grid 'won new-taskbar '() (+ money-reward money) (append record-reward owned))
                                    (Store grid '(clerk task-done) new-taskbar '() (+ money-reward money) (append record-reward owned)))))]
                     ["r" (Store grid '(clerk start) taskbar bag money owned)]
                     [else R])]
                  [else
                   (match input
                     ["r" (Store grid '(clerk start) taskbar bag money owned)]
                     [else R])])])]
       [else (error "invalid mode")])]
    [else (error "not a store")]))