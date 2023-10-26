#lang racket

(define (count-digits num)
  (define (count-divisor n c)
    (if (zero? n) c
        (let-values
            ([(q r) (quotient/remainder n 10)])
          (if (zero? (remainder num r))
              (count-divisor q (add1 c))
              (count-divisor q c)))))
  (count-divisor num 0))

; ============== Test ================
(require rackunit)
(check-equal?
 (count-digits 7) 1)
(check-equal?
 (count-digits 121) 2)
(check-equal?
 (count-digits 1248) 4)