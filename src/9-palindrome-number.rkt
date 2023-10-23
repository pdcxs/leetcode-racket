#lang racket

(define (is-palindrome x)
  (let* ([s (number->string x)]
         [r (list->string (reverse (string->list s)))])
    (string=? s r)))

; ============= Test ==============
(require rackunit)

(check-equal?
 (is-palindrome 121)
 #true)

(check-equal?
 (is-palindrome -121)
 #false)

(check-equal?
 (is-palindrome 10)
 #false)