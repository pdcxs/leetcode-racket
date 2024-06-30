#lang racket

(define (find-target-sum-ways nums target)
  (define input (reverse nums))
  (define (solve input target)
    (if (null? input)
        (if (zero? target) 1 0)
        (let ([remains (rest input)]
              [current (first input)])
          (+ (solve remains (+ current target))
             (solve remains (- current target))))))
  (solve input target))

; ========== Test ============
(require rackunit)
(check-eq?
 (find-target-sum-ways '(1 1 1 1 1) 3) 5)
(check-eq?
 (find-target-sum-ways '(1) 1) 1)
