#lang racket

(define (h-index citations)
  (define sc (sort citations >))
  (define (scan cts m index h)
    (if (null? cts) h
        (let* ([c (first cts)]
               [r (rest cts)]
               [ni (add1 index)]
               [nm (if m (min m c) c)]
               [nh (min ni nm)])
          (if (>= nh h)
              (scan r nm ni nh)
              h))))
  (scan sc #f 0 0))

; =========== Test =============
(require rackunit)
(check-equal?
 (h-index '(3 0 6 1 5)) 3)
(check-equal?
 (h-index '(1 3 1)) 1)