#lang racket

(require "../libs/binomial-heap.rkt")

(define (schedule-course courses)
  ; Here crs is the courses list
  ; sorted by its deadline
  ; in ascending order
  (define (scan crs sum deadline bihp)
    (cond
      ; Finish iterating over all courses
      [(null? crs) (biheap-size bihp)]
      ; If current course will not lead
      ; deadline exceed, select this course
      [(<= (+ (first (first crs)) sum)
           (second (first crs)))
       (scan (rest crs)
             (+ (first (first crs)) sum)
             (second (first crs))
             (biheap-insert
              (first (first crs)) bihp))]
      ; If current course's duration is
      ; less than the maximum
      ; selected duration, select this course
      ; and update the selected durations
      [(< (first (first crs))
           (if (biheap-empty? bihp)
               0 (biheap-min bihp)))
       (scan (rest crs)
             (+ (first (first crs))
                (- sum (biheap-min bihp)))
             (second (first crs))
             (biheap-insert
              (first (first crs))
              (biheap-rm-min bihp)))]
      ; If current course's duration is
      ; greater than the maximum selected
      ; durations and current course selection
      ; will lead deadline exceed,
      ; we will not select current course.
      [else
       (scan (rest crs) sum deadline bihp)]))
  (let ([crs (sort courses <= #:key second)])
    (scan crs 0 0 (make-biheap >=))))

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