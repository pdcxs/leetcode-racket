#lang racket

(define (two-sum nums target)
  (let iter ([l nums] [h (hash)] [i 0])
    (if (hash-has-key? h (car l))
        (list (hash-ref h (car l)) i)
        (iter (cdr l)
              (hash-set h (- target (car l)) i)
              (add1 i)))))

;=========== Test =============
(require rackunit)

(check-equal?
 (two-sum '(2 7 11 15) 9)
 '(0 1))
(check-equal?
 (two-sum '(3 2 4) 6)
 '(1 2))
(check-equal?
 (two-sum '(3 3) 6)
 '(0 1))