#lang racket

(define (relocate-marbles nums from to)
  (define locs (list->set nums))
  (define (scan from to result)
    (if (null? from) (set->list result)
        (let* ([fs (set-remove result (first from))]
               [ts (set-add fs (first to))])
          (scan (rest from) (rest to) ts))))
  (sort (scan from to locs) <))
