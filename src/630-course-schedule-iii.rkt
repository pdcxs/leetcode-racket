#lang racket

(require "../libs/binomial-heap.rkt")

(define (schedule-course courses)
  (define (scan crs sum deadline bihp)
    (cond [(null? crs) (biheap-size bihp)]
          [(and (null? bihp)
                (<= (first (first crs))
                    (second (first crs))))
           (scan (rest crs)
                 (first (first crs))
                 (second (first crs))
                 (biheap-insert
                  (first (first crs)) bihp >=))]
          [(<= (+ (first (first crs)) sum)
               (second (first crs)))
           (scan (rest crs)
                 (+ (first (first crs)) sum)
                 (second (first crs))
                 (biheap-insert
                  (first (first crs)) bihp >=))]
          [(<= (first (first crs))
               (if (null? bihp)
                   0 (biheap-min bihp >=)))
           (scan (rest crs)
                 (+ (first (first crs))
                    (- sum (biheap-min bihp >=)))
                 (second (first crs))
                 (biheap-insert
                  (first (first crs))
                  (biheap-rm-min bihp >=) >=))]
          [else
           (scan (rest crs) sum deadline bihp)]))
  (let ([crs (sort courses <= #:key second)])
    (scan crs 0 0 '())))

; =========== Tests ==========
(require rackunit)
(check-equal?
 (schedule-course
  '((100 200)
    (200 1300)
    (1000 1250)
    (2000 3200)))
 3)

(check-equal?
 (schedule-course
  '((1 2)))
 1)

(check-equal?
 (schedule-course
  '((3 2)
    (4 3)))
 0)