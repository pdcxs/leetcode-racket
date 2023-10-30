#lang racket

; ================ Method 1 ==================
(define (h-index citations)
  (define sc (sort citations >))
  (define (scan cts m index h)
    (if (null? cts) h
        (let* ([c (first cts)]
               [r (rest cts)]
               [ni (add1 index)]
               [nm (if m (min m c) c)]
               [nh (min ni nm)])
          (if (>= nh h)
              (scan r nm ni nh)
              h))))
  (scan sc #f 0 0))

; =============== Method 2 =================
(define (count-h h citations)
  (count (λ (x) (>= x h)) citations))

(define (search left right citations)
  (if (>= left right) left
      (let* ([mid (arithmetic-shift
                    (+ left right 1) -1)]
             [h (count-h mid citations)])
        (if (>= h mid)
            (search mid right citations)
            (search left (sub1 mid) citations)))))

(define (h-index-bisearch citations)
  (let ([l (length citations)])
    (search 0 l citations)))

; =========== Test =============
(require rackunit)
(check-equal?
 (h-index '(3 0 6 1 5)) 3)
(check-equal?
 (h-index '(1 3 1)) 1)
(check-equal?
 (h-index-bisearch '(3 0 6 1 5)) 3)
(check-equal?
 (h-index-bisearch '(1 3 1)) 1)