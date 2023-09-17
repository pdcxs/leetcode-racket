#lang racket

; n can be 1, 2, 3 ... or 1.5, 2.5, 3.5 ...
; get the longest palindrome string
; of s that grows from loation n
; for example
; "cabad" grows from 2 get "aba"
; "cabbad" grows from 2.5 (between 2 and 3)
; get "abba"
; s is the source string
; n is the location
; l is current length of palindrome
(define (center-palindrome s n l)
  (let ([start (inexact->exact (ceiling (- n l)))]
        [end (inexact->exact (floor (+ n l 1)))]
        [sl (string-length s)])
    (if (and (>= start 0)
             (<= end sl)
             (eq? (string-ref s start)
                  (string-ref s (- end 1))))
        (center-palindrome s n (+ l 1))
        (substring s (+ start 1) (- end 1)))))

(define (longer-string s1 s2)
  (if (> (string-length s1) (string-length s2))
      s1 s2))

(define (longest-palindrome s)
  (stream-fold
   longer-string
   ""
   (stream-map (λ (i)
                 (center-palindrome
                  s i
                  (if (= (floor i) i) 0 1)))
               (in-range 0 (string-length s) 0.5))))

; =============== Test ===============
(require rackunit)
(check-equal?
 (longest-palindrome "babad")
 "aba")
(check-equal?
 (longest-palindrome "cbbd")
 "bb")