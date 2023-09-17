#lang racket

; String -> String -> Bool
; Input: source string and pattern
; Output: whether they are matched
; example:
; (is-match "aaaaab" "a*a*a*ab")
; returns true
; (is-match "aaaaab" "a*a*a*a")
; returns false
(define (is-match s p)
  (is-match-lst
   (string->list s)
   (string->list p)
   (mem is-match-lst)))

; Member function result in h
; It's hard to member the compute results
; in a purely functional way. (But it's possible.
; See `memoize` package of Haskell).
; To clarify the idea, I use mutable variable here.
; But with some modification, I avoid using
; global variable to record result.
(define (mem f)
  (let ([memoized (make-hash)])
    (λ (a b mf)
      (let ([key (cons a b)])
        (if (hash-has-key? memoized key)
            (hash-ref memoized key)
            (let ([val (f a b mf)])
              (hash-set! memoized key val)
              val))))))

; ((Char List) (Char List) Hash) -> Bool
; Check whether the list of chars are matched.
; Hash is a hash table to remember the result.
; following expressions should be always equal:
; (is-match-lst
;  (string->list s) (string->list p) h)
; (is-match s p)
(define (is-match-lst s p mem-f)
  (cond ; if the pattern is a null string
    ; the only matched case is
    ; the source string is also a null string
    [(null? p) (null? s)]
    ; If the pattern string only has one char,
    ; or the second char of the pattern string
    ; is not '*',
    ; we should only check the first char.
    [(or (null? (rest p))
         (not (char=? (second p) #\*)))
     ; To check the first char,
     ; we should verify all following conditions are true:
     ; 1. The source string are not empty;
     ; 2. The first char of source string and pattern string
     ;    are matched;
     ; 3. The remaining of source string and pattern string
     ;    are matched.
     (and (not (null? s))
          (match-char
           (first s)
           (first p))
          (mem-f
           (rest s)
           (rest p) mem-f))]
    ; Here, the pattern string are longer than 1
    ; and the second char of pattern string is '*'.
    ; which means here is the repeat pattern
    [else
     ; we need to check at least one of
     ; the following two conditions should be true
     (or
      ; First condition:
      ; source string and pattern string match the
      ; first char
      (and
       ; which requires the source string
       ; chouldn't be empty
       (not (null? s))
       ; and the first char is matched.
       (match-char
        (first s)
        (first p))
       ; In this situation, we need at least one of the following
       ; conditions should be true:
       (or
        ; 1. The first char of the source string
        ;    consumes the repeat pattern, or
        (mem-f
         (rest s) (drop p 2) mem-f)
        ; 2. The first char of the source string
        ;    doesn't consume the repeat pattern.
        (mem-f
         (rest s) p  mem-f)))
      ; Second condition is that the source string
      ; only consumes the repeat pattern
      ; but the source string remains unchanged,
      ; which means the repeat patten
      ; match a zero-time-repeat of the
      ; corresponding char.
      (mem-f
       s (drop p 2) mem-f))]))

; (Char Char) -> Bool
; Check whether the source char
; and pattern char are matched.
; If pattern char is '.'
; they will always matched.
; Otherwise, only equal chars matched.
; Example:
; (match-char #\a #\.) returns true
; (match-char #\a #\a) returns true
; (match-char #\a #\b) returns false
(define (match-char s p)
  (if (char=? p #\.)
      true
      (char=? s p)))

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