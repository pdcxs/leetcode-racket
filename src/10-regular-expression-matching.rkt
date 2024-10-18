#lang racket

(define (is-match s p)
  (let ([r (regexp-match p s)])
    (and r (string=? (first r) s))))

; ================== Test =================
(require rackunit)
(check-equal?
 (is-match "aa" "a")
 #false)
(check-equal?
 (is-match "aa" "a*")
 #true)
(check-equal?
 (is-match "ab" ".*")
 #true)
