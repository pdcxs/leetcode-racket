#lang racket

(define (single-number nums)
  (define r (apply bitwise-xor nums))
  (define pivot (bitwise-and r (- r)))
  (for/fold ([a 0] [b 0] #:result (list a b))
            ([n nums])
    (if (zero? (bitwise-and pivot n))
        (values a (bitwise-xor n b))
        (values (bitwise-xor n a) b))))

; ================== Test ==============
(require rackunit)
(check-equal?
 (sort (single-number '(1 2 1 3 2 5)) <)
 '(3 5))

(check-equal?
 (sort (single-number '(-1 0)) <)
 '(-1 0))

(check-equal?
 (sort (single-number '(0 1)) <)
 '(0 1))