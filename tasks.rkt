#lang racket

(require 2htdp/image
         "basics.rkt")

(provide check-tasks
         Task
         sampled-by-any?

         find-k-albums-by
         buy-k-albums
         buy-k-albums-with)

(struct Task [text reward test?])

;; macros to make generic tasks

(define-syntax buy-k-albums
  (syntax-rules ()
    ((_ k)
     (λ (bag owned)
       (>= (length owned) k)))))

(define-syntax find-k-albums-by
  (syntax-rules ()
    ((_ k artist)
     (λ (bag owned)
       (let ((relevant
              (filter
               (λ (r) (string=? (Record-artist r) artist))
               (append bag owned))))
         (>= (length relevant) k))))))

(define-syntax buy-k-albums-with
  (syntax-rules ()
    ((_ k artist sym)
     (λ (bag owned)
       (let ((relevant
              (filter (λ (r) (or (string=? artist (Record-artist r))
                                 (member sym (map car (Record-facts r))))) owned)))
         (>= (length relevant) k))))))

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