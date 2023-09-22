#lang racket

(define (dist-money money children)
  (cond [(< money children) -1]
        [(> money (* children 8)) (sub1 children)]
        [(= money (- (* 8 children) 4))
         (- children 2)]
        [else
         (floor (/ (- money children) 7))]))

; ============= Test ===============
(require rackunit)
(check-equal?
 (dist-money 20 3) 1)
(check-equal?
 (dist-money 16 2) 2)
(check-equal?
 (dist-money 2 2) 0)
(check-equal?
 (dist-money 13 3) 1)
(check-equal?
 (dist-money 19 2) 1)