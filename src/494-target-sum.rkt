#lang racket

(define (find-target-sum-ways nums target)
  (define (mem func)
    (let ([results (make-hash)])
      (λ (x y)
        (let ([key (cons x y)])
          (if (hash-has-key? results key)
              (hash-ref results key)
              (let ([val (func x y)])
                (hash-set! results key val)
                val))))))
  (define (solve input target)
    (if (null? input)
        (if (zero? target) 1 0)
        (let ([remains (rest input)]
              [current (first input)])
          (+ (mem-solve remains (+ current target))
             (mem-solve remains (- current target))))))
  (define mem-solve (mem solve))
  (solve nums target))

; ========== Test ============
(require rackunit)
(check-eq?
 (find-target-sum-ways '(1 1 1 1 1) 3) 5)
(check-eq?
 (find-target-sum-ways '(1) 1) 1)
