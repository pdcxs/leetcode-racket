#lang racket

(define (relocate-marbles nums from to)
  (define locs (list->set nums))
  (define (scan from to result)
    (if (null? from) (set->list result)
        (let* ([fs (set-remove result (first from))]
               [ts (set-add fs (first to))])
          (scan (rest from) (rest to) ts))))
  (sort (scan from to locs) <))


; ================== Test ==========================
(require rackunit)

(check-equal?
 (relocate-marbles '(1 6 7 8) '(1 7 2) '(2 9 5))
 '(5 6 8 9))
(check-equal?
 (relocate-marbles '(1 1 3 3) '(1 3) '(2 2))
 '(2))
