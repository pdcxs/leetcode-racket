#lang racket

(define (max-product words)
  (let ([set-dict (scan words '())])
    (count-product set-dict)))

(define (scan words lst)
  (if (null? words) lst
      (let ([w (first words)])
        (scan
         (rest words)
         (cons
          (cons
           (list->set
            (string->list w))
           (string-length w))
          lst)))))

(define (count-product set-dict)
  (apply
   max (prod-lst set-dict)))

(define (prod-lst set-dict)
  (cond
    [(or
      (null? set-dict)
      (null? (cdr set-dict)))
     '(0)]
    [else
     (cons (prod set-dict)
           (prod-lst
            (rest set-dict)))]))

(define (prod set-dict)
  (let ([f (first set-dict)]
        [r (rest set-dict)])
    (if (null? r) (cdr f)
        (apply
         max
         (map
          (λ (d)
            (if
             (set-empty?
              (set-intersect
               (car f)
               (car d)))
             (* (cdr f) (cdr d))
             0)) r)))))

; ================ Test =================
(require rackunit)
(check-eq?
 (max-product '("abcw"
                "baz"
                "foo"
                "bar"
                "xtfn"
                "abcdef"))
 16)
(check-eq?
 (max-product '("a"
                "ab"
                "abc"
                "d"
                "cd"
                "bcd"
                "abcd"))
 4)
(check-eq?
 (max-product '("a"
                "aa"
                "aaa"
                "aaaa"))
 0)