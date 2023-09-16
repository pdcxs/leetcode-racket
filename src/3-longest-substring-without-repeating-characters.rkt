#lang racket

(define (length-of-longest-substring s)
  (define l (string-length s))
  ; Get a longest unrepeated substring
  ; of string s starting from n.
  ; Use hash-table rather than string-contains?
  ; to speed up the lookup operation
  (define (unrepeat s n)
    (let loop ([i n]
               [h (hash)])
      (if (>= i l) (- l n)
          (let ([c (substring s i (add1 i))])
            (if (hash-has-key? h c)
                (- i n)
                (loop (add1 i)
                      (hash-set h c #t)))))))

  ; Use stream to avoid large memory usage.
  (stream-fold max 0
               (stream-map
                (λ (i) (unrepeat s i))
                (in-range l))))

; =========== Tests =============
(require rackunit)

(check-equal?
 (length-of-longest-substring "abcabcbb")
 3 "input 1")
(check-equal?
 (length-of-longest-substring "bbbbb")
 1 "input 2")
(check-equal?
 (length-of-longest-substring "pwwkew")
 3 "input 3")