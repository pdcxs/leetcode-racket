#lang racket

(define (num-rolls-to-target n k target)
  (cond
    [(and (zero? n) (zero? target)) 1]
    [(negative? target) 0]
    [(> target (* n k)) 0]
    ; sum all possibles
    [else
     (remainder
      (apply + (for/list
                   ([i (inclusive-range 1 k)])
                 (mem-num-rolls-to-target
                  (sub1 n) k (- target i))))
      1000000007)]))

(define (mem f)
  (let ([memoized (make-hash)])
    (λ (n k target)
      (let ([key (list n k target)])
        (if (hash-has-key? memoized key)
            (hash-ref memoized key)
            (let ([val (f n k target)])
              (hash-set! memoized key val)
              val))))))

(define mem-num-rolls-to-target
  (mem num-rolls-to-target))

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