#lang racket

; Very simple question.
(define (min-count coins)
  (apply + (map (λ (x) (ceiling (/ x 2))) coins)))

; ========= Test ===========
(require rackunit)
(check-equal?
 (min-count '(4 2 1)) 4)
(check-equal?
 (min-count '(2 3 10)) 8)