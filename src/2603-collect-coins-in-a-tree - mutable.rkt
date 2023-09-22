#lang racket

; A graph is needed.
; A graph is a hash-table
; key is node
; value is its neighbors
; The hash-table based method is too slow,
; so I tried vector, but it exceeds
; memory limitation. So I use mutable
; hash-table here. But this is even worse.
; I give up, optimizing the performance
; of functional program is too hard for me.

(define (build-graph n edges)
  (define graph (make-hash))
  (define degree (make-vector n))
  (define (scan edges)
    (unless (null? edges)
      (let* ([n1 (first (first edges))]
             [n2 (second (first edges))]
             [nb1 (vector-append (vector n2) (hash-ref graph n1 (vector)))]
             [nb2 (vector-append (vector n1) (hash-ref graph n2 (vector)))])
        (begin
          (hash-set! graph n1 nb1)
          (hash-set! graph n2 nb2)
          (vec-add1! degree n1)
          (vec-add1! degree n2)
          (scan (rest edges))))))
  (scan edges) (cons graph degree))

(define (vec-add1! vec index)
  (vector-set!
   vec index
   (add1 (vector-ref vec index))))

(define (vec-sub1! vec index)
  (vector-set!
   vec index
   (sub1 (vector-ref vec index))))

; remove leaf nodes with zero coin
(define (remove-zero-leaf! graph degree coins)
  (define need-removed '())
  (define n (vector-length coins))
  (define (get-removed-nodes!)
    (for ([i (range n)])
      (when
          (and
           (= (vector-ref degree i) 1)
           (= (vector-ref coins i) 0))
        (set! need-removed (cons i need-removed))
        (vec-sub1! degree i)
        (for ([j (hash-ref graph i '())])
          (vec-sub1! degree j)))))

  (get-removed-nodes!)
  (unless (null? need-removed)
    (remove-nodes! graph need-removed)
    (remove-zero-leaf! graph degree coins))
  graph)

; remove a node in a graph
(define (remove-node! graph node)
  (hash-remove! graph node)
  (let loop ([i (hash-iterate-first graph)])
    (if i
        (begin
          (hash-update!
           graph (hash-iterate-key graph i)
           (λ (nb) (vector-filter
                    (λ (n) (not (= n node)))
                    nb)))
          (loop
           (hash-iterate-next graph i)))
        graph)))

; remove a list of nodes
(define (remove-nodes! graph nodes)
  (if (null? nodes)
      graph
      (begin
        (remove-node! graph (first nodes))
        (remove-nodes!
         graph
         (rest nodes)))))

; delete all leaf nodes
(define (delete-leaf! graph degree)
  (define leaves
    (filter
     (λ (i) (= 1 (vector-ref degree i)))
     (range (vector-length degree))))
  (for ([leaf leaves])
    (begin
      (vec-sub1! degree leaf)
      (let ([father
             (vector-ref
              (hash-ref graph leaf) 0)])
        (vec-sub1! degree father))))
  (remove-nodes! graph leaves))

; Main function
(define (collect-the-coins coins edges)
  (define cs (list->vector coins))
  (define n (vector-length cs))
  (define gd (build-graph n edges))
  (define graph (car gd))
  (define degree (cdr gd))
  (remove-zero-leaf! graph degree cs)
  (delete-leaf! graph degree)
  (delete-leaf! graph degree)
  (let ([node-num (hash-count graph)])
    (if (zero? node-num) 0
        (- (* 2 node-num) 2))))

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