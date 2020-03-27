#lang racket

(require 2htdp/image
         "basics.rkt")

(provide check-tasks
         Task
         sampled-by-any?)

(struct Task [text reward test?])


(define (sampled-by-any? r rs)
  (let ((title (Record-title r)))
    (ormap
     (λ (r2) (member `(samples ,title) (Record-facts r2)))
     rs)))

;; task checking
(define (check-tasks taskbar bag owned)
  (let loop ((leftovers '())
             (money-reward 0)
             (record-reward '())
             (taskbar taskbar))
  (cond
    [(null? taskbar)
     (values money-reward record-reward (reverse leftovers))]
    [else
     (match (car taskbar)
       [(Task text reward test?)
        (let ((v (test? bag owned)))
            (if v
                (loop leftovers (+ (car reward) money-reward) (append (cdr reward) record-reward) (cdr taskbar))
                (loop (cons (car taskbar) leftovers) money-reward record-reward (cdr taskbar))))])])))