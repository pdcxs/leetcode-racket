#lang racket

; A graph is needed.
; A graph is a vector
; graph[i] is a set of nodes which are
; neighbors of node i.
; Note: this solution is very ugly,
; but it works.

(define (vector-add1! vec index)
  (vector-set!
   vec index
   (add1 (vector-ref vec index))))

(define (vector-sub1! vec index)
  (vector-set!
   vec index
   (sub1 (vector-ref vec index))))

(define (collect-the-coins cs edges)
  (define coins (list->vector cs))
  (define n (vector-length coins))

  ; initial graph and degree vector
  (define graph (build-vector n (λ (i) (mutable-set))))
  (define degree (make-vector n))

  ; Build graph
  (for ([e edges])
    (let ([n1 (first e)]
          [n2 (second e)])
      (set-add!
       (vector-ref graph n1) n2)
      (set-add!
       (vector-ref graph n2) n1)
      (vector-add1! degree n1)
      (vector-add1! degree n2)))

  (define queue (make-vector n))
  (define queue-head 0)
  (define queue-tail 0)
  (define rest n)

  ; delete all leaves without coin
  (for ([i (in-range n)])
    (when (and
           (is-leaf degree i)
           (zero? (vector-ref coins i)))
      (vector-set! queue queue-tail i)
      (set! queue-tail (add1 queue-tail))))

  (for ([i (in-naturals)]
        #:break (= queue-head queue-tail))
    (let ([u (vector-ref queue queue-head)])
      (set! queue-head (add1 queue-head))
      (vector-sub1! degree u)
      (set! rest (sub1 rest))
      (for ([v (vector-ref graph u)])
        (vector-sub1! degree v)
        (when
            (and (is-leaf degree v)
                 (zero? (vector-ref coins v)))
          (vector-set! queue queue-tail v)
          (set! queue-tail (add1 queue-tail))))))
  
  ; delete twice leaves
  (for ([j (in-range 2)])
    (set! queue-head 0)
    (set! queue-tail 0)
    (for ([i (in-range n)])
      (when (is-leaf degree i)
        (vector-set! queue queue-tail i)
        (set! queue-tail (add1 queue-tail))))
    (for ([i (in-naturals)]
          #:break (= queue-head queue-tail))
      (let ([u (vector-ref queue queue-head)])
        (set! queue-head (add1 queue-head))
        (vector-sub1! degree u)
        (set! rest (sub1 rest))
        (for ([v (vector-ref graph u)])
          (vector-sub1! degree v)))))
  (if (= 0 rest) 0
      (* 2 (sub1 rest))))

(define (is-leaf degree i)
  (= 1 (vector-ref degree i)))

; ============= test =====================
(require rackunit)
(check-equal?
 (collect-the-coins
  '(1 0 0 0 0 1)
  '((0 1) (1 2) (2 3) (3 4) (4 5)))
 2)

(check-equal?
 (collect-the-coins
  '(0 0 0 1 1 0 0 1)
  '((0 1) (0 2) (1 3) (1 4) (2 5) (5 6) (5 7)))
 2)

(check-equal?
 (collect-the-coins
  '(0 1)
  '((0 1)))
 0)

(check-equal?
 (collect-the-coins
  '(1 0 0 1 1 0 0 0 0 1 0 0)
  '((0 1) (1 2) (1 3) (2 4) (4 5) (5 6)
          (5 7) (4 8) (7 9) (7 10) (10 11)))
 4)