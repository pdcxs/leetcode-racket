#lang racket

; Bottom Up Merge Sort (bums)
; provide add and sort
; add takes O(log N) time
; sort takes O(N) time

; The structure of the data type is
; (list
;  less -> the less compare function
;  size -> number
;  segments -> stream of list)
(define (make-bums [less <])
  (list less 0 empty-stream))

; Define the getters
(define bums-less first)
(define bums-size second)
(define bums-segs third)

; merge less is a function
; merge 2 sorted lists into one
; here we use currify style
(define ((merge less) lst1 lst2)
  (cond [(null? lst2) lst1]
        [(null? lst1) lst2]
        [(less (car lst1) (car lst2))
         (cons
          (car lst1)
          ((merge less)
           (cdr lst1) lst2))]
        [else
         (cons
          (car lst2)
          ((merge less)
           lst1 (cdr lst2)))]))

; Add an element to a bums
(define (bums-add a bums)
  (define size (bums-size bums))
  (define less (bums-less bums))
  (define (add-seg seg segs size)
    (if (even? size)
        (stream-cons seg segs)
        (add-seg
         ((merge less)
          seg (stream-first segs))
         (stream-rest segs)
         (arithmetic-shift size -1))))
  (list
   (bums-less bums)
   (add1 size)
   (stream-lazy
    (add-seg (list a)
             (bums-segs bums)
             size))))

; sort the bums
(define (bums-sort bums)
  (define less (bums-less bums))
  (stream-fold (merge less) '()
               (bums-segs bums)))

; ======== test ==========
(require rackunit)

(let ([input
       (list
        (list 4 3 2 1)
        (list 1 2 3 4 5)
        (list 5 3 4 6 1 4 5)
        (range 1 100))])
  (for ([i input])
    (check-equal?
     (bums-sort
      (foldl bums-add
             (make-bums) i))
     (sort i <))))