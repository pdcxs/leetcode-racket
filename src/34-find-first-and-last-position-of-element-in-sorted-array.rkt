#lang racket

(define (bisearch nums target lower upper)
  ; middle is (floor (quotient (+ lower upper) 2))
  ; use bitwise shift to speed up
  (define mid (arithmetic-shift (+ lower upper) -1))
  (let ([m (list-ref nums mid)])
    (cond
      ; return lower doesn't means we find target
      ; it means lower is the largest index i
      ; that satisfy:
      ; (< (list-ref nums i) target)
      ; for example
      ; if we search 3 in '(1 2 4 5 6)
      ; then we will get 1
      [(> lower upper) lower]
      [(< m target)
       (bisearch nums target (add1 mid) upper)]
      [(= mid 0) lower]
      [else
       (bisearch nums target lower (sub1 mid))])))

(define (search-range nums target)
  (if
   (null? nums) '(-1 -1)
   (let* ([l (length nums)]
          [lower (bisearch nums target
                           0 (sub1 l))]
          ; because nums is a list of integers
          ; increase one of the target
          ; should be the index of the right
          ; of the last index of target
          [upper (sub1
                  (bisearch nums
                            (add1 target)
                            0 (sub1 l)))])
     (if (or (>= lower l)
             (not (= (list-ref nums lower) target)))
         '(-1 -1) (list lower upper)))))

; ============ Test ==================
(require rackunit)

(check-equal?
 (search-range '(5 7 7 8 8 10) 8)
 '(3 4))

(check-equal?
 (search-range '(5 7 7 8 8 10) 6)
 '(-1 -1))

(check-equal?
 (search-range '() 0)
 '(-1 -1))

(check-equal?
 (search-range '(1) 1)
 '(0 0))