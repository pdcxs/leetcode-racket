#lang racket

(define (insert lst idx color)
  (if (= idx 0)
      (cons (set-add (first lst) color)
            (rest lst))
      (cons (first lst)
            (insert (rest lst)
                    (sub1 idx) color))))

(define (get-idx c)
  (- (char->integer c)
     (char->integer #\0)))

(define (collect-color lst color-sets)
  (match lst
    ['() (count
          (λ (s)
            (= (set-count s) 3))
          color-sets)]
    [(cons color (cons ring r))
     (collect-color
      r (insert color-sets
                (get-idx ring) color))]))

(define (count-points rings)
  (collect-color (string->list rings)
                 (build-list 10 (λ (_) (set)))))