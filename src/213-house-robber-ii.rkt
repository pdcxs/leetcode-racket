#lang racket

; First assuming hourses is in a line
; Maximum of first n nums
; is max of:
; rob n?: (list-ref nums n) + rob (n - 2)
; not rob n? rob (n - 1)
; Then we need to deal with the ring.
; If rob first hourse
; it is the same as robbing a hourse line
; without first two hourses and last hourse
; Otherwise is the same as robbing a hourse line
; without the first first hourse.
; This is a dynamic problem, should use
; function memoization.

; function memoization
(define (mem f)
  (define r (make-hash))
  (λ (x)
    (if (hash-has-key? r x)
        (hash-ref r x)
        (let ([v (f x)])
          (hash-set! r x v)
          v))))

(define (rob-line nums)
  (define (best-rob i)
    (cond
      [(null? nums) 0]
      [(= i 0) (first nums)]
      [(= i 1) (max (first nums)
                    (second nums))]
      [else
       (max
        ; robbing i-th hourse
        (+
         (list-ref nums i)
         (mem-best-rob
          (- i 2)))
        ; or not
        (mem-best-rob (- i 1)))]))
  (define mem-best-rob (mem best-rob))
  (best-rob (sub1 (length nums))))

; get a list without last element
(define (most lst)
  (if (null? (rest lst)) '()
      (cons (first lst) (most (rest lst)))))

(define (rob nums)
  (define l (length nums))
  (cond [(= l 0) 0]
        [(= l 1) (first nums)]
        [(= l 2)
         (apply max nums)]
        [else
         (max
          ; rob first hourse
          (+ (first nums)
             (rob-line
              (most (cddr nums))))
          ; or not
          (rob-line (rest nums)))]))

; ================ test ==================
(require rackunit)
(check-equal?
 (rob '(2 3 2)) 3)
(check-equal?
 (rob '(1 2 3 1)) 4)
(check-equal?
 (rob '(1 2 3)) 3)