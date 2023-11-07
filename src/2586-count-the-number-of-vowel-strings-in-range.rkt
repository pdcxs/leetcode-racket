#lang racket

(define (vowel-strings words left right)
  (count-vowel (drop words left)
               (- right left)))

(define (count-vowel words most-num)
  (define (scan words idx)
    (cond
      [(null? words) 0]
      [(> idx most-num) 0]
      [(is-vowel-word? (first words))
       (add1 (scan (rest words)
                   (add1 idx)))]
      [else
       (scan (rest words)
             (add1 idx))]))
  (scan words 0))

(define (is-vowel-word? word)
  (and (is-vowel
        (string-ref word 0))
       (is-vowel
        (string-ref
         word
         (sub1 (string-length word))))))

(define vowels (set #\a #\e #\i #\o #\u))

(define (is-vowel c)
  (set-member? vowels c))

; ================ Test =====================
(require rackunit)
(check-eq?
 (vowel-strings '("are" "amy" "u") 0 2)
 2)
(check-eq?
 (vowel-strings '("hey"
                  "aeo"
                  "mu"
                  "ooo"
                  "artro") 1 4)
 3)