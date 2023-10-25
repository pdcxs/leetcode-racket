#lang racket

; I try use stream to remember the results
; but its time-out

(define (num-rolls-to-target n k target)
  (define ((roll-num mem-f) n target)
    (cond
      [(and (zero? n) (zero? target)) 1]
      [(negative? target) 0]
      [(> target (* n k)) 0]
      ; sum all possibles
      [else
       (remainder
        (apply + (for/list
                     ([i (inclusive-range 1 k)])
                   (mem-f
                    (sub1 n) (- target i))))
        1000000007)]))
  (define (mem-f x y)
    (if (or (negative? x)
            (negative? y))
        0
        (stream-ref
         (stream-ref results x) y)))
  (define results
    (stream-map
     (λ (x) (stream-map
             (λ (y) ((roll-num mem-f) x y))
             (in-naturals)))
     (in-naturals)))
  (mem-f n target))

; ================== Test ===========
(require rackunit)
(check-equal?
 (num-rolls-to-target 1 6 3) 1)
(check-equal?
 (num-rolls-to-target 2 6 7) 6)
(check-equal?
 (num-rolls-to-target 30 30 500)
 222616187)
(check-equal?
 (num-rolls-to-target 2 5 10) 1)