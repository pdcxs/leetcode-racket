#lang racket

(define (get-zeros str-lst)
  (if (or
       (null? str-lst)
       (char=? (first str-lst) #\1))
      (values 0 str-lst)
      (let-values
          ([(len lst)
            (get-zeros (rest str-lst))])
        (values (add1 len) lst))))

(define (get-ones str-lst max-num)
  (define (scan lst idx)
    (if (or
         (>= idx max-num)
         (null? lst)
         (char=? (first lst) #\0))
        (values (* 2 idx) lst)
        (scan (rest lst) (add1 idx))))
  (scan str-lst 0))

(define (drop-ones str-lst)
  (if (or
       (null? str-lst)
       (char=? (first str-lst) #\0))
      str-lst
      (drop-ones (rest str-lst))))

(define (find-the-longest-balanced-substring s)
  (define (scan lst max-len)
    (if (null? lst) max-len
        (let ([l1 (drop-ones lst)])
          (let*-values
           ([(len pt)
             (get-zeros l1)]
            [(n p)
             (get-ones pt len)])
           (if (> n max-len)
               (scan p n)
               (scan p max-len))))))
  (scan (string->list s) 0))

; =============== Test ===================
(require rackunit)
(check-equal?
 (find-the-longest-balanced-substring
  "01000111") 6)
(check-equal?
 (find-the-longest-balanced-substring
  "00111") 4)
(check-equal?
 (find-the-longest-balanced-substring
  "111") 0)
(check-equal?
 (find-the-longest-balanced-substring
  "0") 0)