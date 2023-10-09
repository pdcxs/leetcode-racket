#lang racket

(define (get-digits num)
  (define (scan n d)
    (if (= n 0) d
        (scan (floor (/ n 10))
              (cons (remainder n 10) d))))
  (sort (scan num '()) <))

(define (split-num num)
  (let loop ([n1 0]
             [n2 0]
             [d (get-digits num)]
             [i 0])
    (cond
      [(null? d) (+ n1 n2)]
      [(zero? i)
       (loop (+ (* 10 n1) (first d)) n2 (rest d) 1)]
      [else
       (loop n1 (+ (* 10 n2) (first d)) (rest d) 0)])))

; =============== Test =============
(require rackunit)

(check-eq?
 (split-num 4325) 59)

(check-eq?
 (split-num 687) 75)

(check-eq?
 (split-num 671835844) 18046)