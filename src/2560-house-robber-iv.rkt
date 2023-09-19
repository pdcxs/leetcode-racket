#lang racket

(define (min-capability nums k)
  ; maximum number of houses can be robbed
  ; if max money of each house is not
  ; greater than m
  (define (max-house m)
    (define (scan nums is-robbed count)
      (cond
        [(null? nums) count]
        [(and (<= (first nums) m)
              (not is-robbed))
         (scan (rest nums) #true (add1 count))]
        [else
         (scan (rest nums) #false count)]))
    (scan nums #false 0))
  (define-values (l u) (min/max nums))
  ; Serch answer: low bound and upper bound
  ; the answer must be in the nums
  ; which is explained in problem 34.
  (define (search l u)
    (if (> l u) l
        (let* ([m (arithmetic-shift (+ l u) -1)]
               [c (max-house m)])
          (if (>= c k)
              (search l (sub1 m))
              (search (add1 m) u)))))
  (search l u))

; find minimum and maximum of a list
(define (min/max lst)
  (define (update x acc)
    (cond [(< x (car acc))
           (cons x (cdr acc))]
          [(> x (cdr acc))
           (cons (car acc) x)]
          [else acc]))
  (let ([r (foldl update (cons 0 0) lst)])
    (values (car r) (cdr r))))
  
; ================= Test =================
(require rackunit)

(check-equal?
 (min-capability
  '(2 3 5 9) 2) 5)

(check-equal?
 (min-capability
  '(2 7 9 3 1) 2) 2)
