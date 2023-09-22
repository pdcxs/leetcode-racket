#lang racket

(define min-int -2147483648)
(define max-int 2147483647)

(define (limit x)
  (if (<= min-int x max-int)
      x
      0))

(define (list-reverse x)
  (if (null? x)
      x
      (append (list-reverse (cdr x)) (list (car x)))))

(define (sign x)
  (if (> x 0)
      1
      -1))

(define (reverse x)
  (let* ([s (number->string (abs x))]
         [l (string->list s)]
         [rl (list-reverse l)]
         [rs (list->string rl)]
         [n (string->number rs)])
   (limit (* (sign x) n))))

; ============= Test =============
(require rackunit)
(check-equal?
 (reverse 123) 321)
(check-equal?
 (reverse -123) -321)
(check-equal?
 (reverse 120) 21)
(check-equal?
 (reverse 0) 0)