#lang racket

; Insert Sort Operation
(define (insert x xs)
  (match xs
    ['() (list x)]
    [(cons y ys)
     #:when (>= x y)
     (cons x xs)]
    [(cons y ys)
     (cons y (insert x ys))]))

(define (pick-gifts gifts k)
  (define sorted-gifts (sort gifts >))
  (define (pick gifts k)
    (if (zero? k)
        (apply + gifts)
        (let ([m (first gifts)]
              [r (rest gifts)])
          (pick (insert
                 (inexact->exact
                  (floor (sqrt m)))
                 r)
                (sub1 k)))))
  (pick sorted-gifts k))

; ============= Test =============
(require rackunit)
(check-equal?
 (pick-gifts '(25 64 9 4 100) 4)
 29)
(check-equal?
 (pick-gifts '(1 1 1 1) 4)
 4)