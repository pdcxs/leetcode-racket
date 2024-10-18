#lang racket

(define (two-sum nums target [index 0] [mem (hash)])
  (let* ([n (first nums)]
         [other-num (- target n)])
    (if (hash-has-key? mem other-num)
        (list (hash-ref mem other-num) index)
        (two-sum (rest nums) target (add1 index)
                 (hash-set mem n index)))))

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
