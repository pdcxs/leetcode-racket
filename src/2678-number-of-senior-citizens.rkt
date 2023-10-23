#lang racket

(define (get-age str)
  (string->number
   (substring str 11 13)))

; We can use recursion to avoid
; creating inter-list.
(define (count-seniors details)
  (match details
    ['() 0]
    [(cons x xs)
     #:when (> (get-age x) 60)
     (+ 1 (count-seniors (rest details)))]
    [else
     (count-seniors (rest details))]))

; Normal way to solve this problem,
; but a little bit slower than the
; recursion method.
(define (count-seniors-normal details)
  (length
   (filter (λ (age) (> age 60))
           (map get-age details))))

; We can use stream to speed up
; but interesting, it's slower
; than the normal way.
(define (count-seniors-lazy details)
  (stream-length
   (stream-filter
    (λ (age) (> age 60))
    (for/stream ([d details])
      (get-age d)))))

; ============= Test ==============
(require rackunit)

(check-equal?
 (count-seniors
  (list "7868190130M7522"
        "5303914400F9211"
        "9273338290F4010"))
 2)

(check-equal?
 (count-seniors
  (list "1313579440F2036"
        "2921522980M5644"))
 0)