#lang racket

; get like-time of the satisfaction list
(define (like-time satis)
  (apply
   + (for/list
         ([s satis]
          [j (in-naturals 1)])
       (* s j))))

; get all like-time
(define (list-like-time satis)
  (cond
    [(null? satis) '()]
    [(negative? (first satis))
     (cons (like-time satis)
           (list-like-time (rest satis)))]
    [else
     (list (like-time satis))]))

(define (ramp v)
  (if (positive? v) v 0))

; main function
(define (max-satisfaction satisfaction)
  (ramp
   (apply max
          (list-like-time
           (sort satisfaction <)))))

; ============== Test ===================
(require rackunit)
(check-equal?
 (max-satisfaction '(-1 -8 0 5 -9))
 14)

(check-equal?
 (max-satisfaction '(4 3 2))
 20)

(check-equal?
 (max-satisfaction '(4 3 2))
 20)

(check-equal?
 (max-satisfaction '(-1 -4 -5))
 0)