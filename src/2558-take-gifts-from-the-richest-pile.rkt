#lang racket

; ==================== Method 1 ====================
; Use insert sort

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

; ==================== Method 2 ====================
; Use binomial heap, i.e priority queue.

(require "../libs/binomial-heap.rkt")

(define (pick-gifts-heap gifts k)
  (define heap (list->biheap gifts >=))
  (define total (apply + gifts))
  (define (pick gifts k s)
    (if (zero? k)
        (- total s)
        (let* ([m (biheap-min gifts)]
               [r (biheap-rm-min gifts)]
               [p (inexact->exact (floor (sqrt m)))]
               [g (- m p)])
          (pick (biheap-insert p r)
                (sub1 k) (+ s g)))))
  (pick heap k 0))

; ============= Test =============
(require rackunit)

(check-equal?
 (pick-gifts '(25 64 9 4 100) 4)
 29)
(check-equal?
 (pick-gifts '(1 1 1 1) 4)
 4)

(check-equal?
 (pick-gifts-heap '(25 64 9 4 100) 4)
 29)
(check-equal?
 (pick-gifts-heap '(1 1 1 1) 4)
 4)