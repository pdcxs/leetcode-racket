#lang racket

(define (all-partition str size)
  (if
   (string=? str "") '(())
   (let ([s (min size (string-length str))])
     (for*/list
         ([i (range 1 (add1 s))]
          [pts (all-partition (substring str i) size)])
       (cons (substring str 0 i) pts)))))

(define (is-punish n)
  (define sq (* n n))
  (define sqn (number->string sq))
  (define pts (all-partition
               sqn
               (string-length
                (number->string n))))
  (ormap
   (λ (pt)
     (= (apply + (map string->number pt)) n))
   pts))

; Interesting, use global memoization can
; make the program much faster.
; We can find the testing method of leetcode
; with this example.
(define results (make-hash))
(define (mem-is-punish n)
  (if (hash-has-key? results n)
      (hash-ref results n)
      (let ([val (is-punish n)])
        (hash-set! results n val)
        val)))

(define (punishment-number n)
  (apply
   + (map (λ (x) (* x x))
          (filter mem-is-punish
                  (range 1 (add1 n))))))

; ================ Test ===================

(require rackunit)
(check-equal?
 (punishment-number 10) 182)
(check-equal?
 (punishment-number 37) 1478)