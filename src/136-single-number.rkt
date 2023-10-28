#lang racket

; =============== Method 1 ===============
; use hash table

(define (single-number nums)
  (define (count-nums nums h)
    (if (null? nums)
        (find-one-time h)
        (let* ([f (first nums)]
               [r (rest nums)]
               [c (hash-ref h f 0)])
          (count-nums r
                      (hash-set h f (add1 c))))))
  (count-nums nums (hash)))

(define (find-one-time h)
  (define (scan pos)
    (if (= (hash-iterate-value h pos) 1)
        (hash-iterate-key h pos)
        (scan (hash-iterate-next h pos))))
  (scan (hash-iterate-first h)))

; ============= Method 2 =============
; use bitwise xor
(define (single-number-xor nums)
  (apply bitwise-xor nums))

; ========== Test ===========
(require rackunit)

(check-equal?
 (single-number '(2 2 1)) 1)

(check-equal?
 (single-number '(4 1 2 1 2)) 4)

(check-equal?
 (single-number '(1)) 1)

(check-equal?
 (single-number-xor '(2 2 1)) 1)

(check-equal?
 (single-number-xor '(4 1 2 1 2)) 4)

(check-equal?
 (single-number-xor '(1)) 1)