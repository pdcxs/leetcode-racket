#lang racket

(define (find-repeated-dna-sequences s)
  (define n (- (string-length s) 10))
  (let loop ([i 0]
             [h (hash)])
    (if (> i n) (repeated-dna h)
        (let ([subs (substring s i (+ i 10))])
          (loop (add1 i)
                (hash-set h subs
                          (add1 (hash-ref h subs 0))))))))

(define (repeated-dna h)
  (define (scan p lst)
    (if (not p) lst
        (let ([next (hash-iterate-next h p)])
          (if (> (hash-iterate-value h p) 1)
              (scan next (cons (hash-iterate-key h p) lst))
              (scan next lst)))))
  (scan (hash-iterate-first h) '()))

; ====================== Test ========================
(require rackunit)
(check-equal?
 (find-repeated-dna-sequences "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT")
 '("AAAAACCCCC" "CCCCCAAAAA"))
(check-equal?
 (find-repeated-dna-sequences "AAAAAAAAAAAAA")
 '("AAAAAAAAAA"))