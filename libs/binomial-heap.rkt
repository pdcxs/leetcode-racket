#lang racket

(provide
 biheap-insert
 biheap-merge
 biheap-min
 biheap-rm-min
 biheap-size)

; Binomial Heap (biheap)
; Also known as Priority Queue

; We also need a tree first
; A tree is a node with the
; following structure:
; (list
;  rank: integer
;  root: value of current node
;  list of tree (children)
; a heap is a list of trees

; Node constructor
(define (make-node rank val chld)
  (list rank val chld))

; Define the getters
(define node-rank first)
(define node-root second)
(define node-chld third)

; Basic Operation: Link two trees
; with same ranks
(define (link tree1 tree2 [leq <=])
  (let ([r (node-rank tree1)]
        [x1 (node-root tree1)]
        [x2 (node-root tree2)])
    (if (leq x1 x2)
        (make-node
         (add1 r) x1
         (cons tree2 (node-chld tree1)))
        (make-node
         (add1 r) x2
         (cons tree1 (node-chld tree2))))))

; insert a tree into a heap
(define (insert-tree t ts [leq <=])
  (if (null? ts)
      (list t)
      (let ([h (first ts)])
        (if (< (node-rank t) (node-rank h))
            (cons t ts)
            (insert-tree (link t h leq)
                         (rest ts) leq)))))

; insert a value into a binomial heap
(define (biheap-insert x ts [leq <=])
  (insert-tree (make-node 0 x '()) ts leq))

; merge two biheaps
(define (biheap-merge ts1 ts2 [leq <=])
  (cond [(null? ts1) ts2]
        [(null? ts2) ts1]
        [(< (node-rank (first ts1))
            (node-rank (first ts2)))
         (cons (first ts1)
               (biheap-merge
                (rest ts1) ts2 leq))]
        [(< (node-rank (first ts2))
            (node-rank (first ts1)))
         (cons (first ts2)
               (biheap-merge
                ts1 (rest ts2) leq))]
        [else
         (insert-tree
          (link (first ts1)
                (first ts2) leq)
          (biheap-merge (rest ts1) (rest ts2) leq)
          leq)]))

; find minimum element in a biheap
(define (biheap-min ts [leq <=])
  (if (null? (rest ts)) ; only has one tree
      (node-root (first ts))
      (let ([x (node-root (first ts))]
            [y (biheap-min (rest ts) leq)])
        (if (leq x y) x y))))

; delete minimum element in a biheap
(define (biheap-rm-min ts [leq <=])
  (define (get-min ts)
    (if (null? (rest ts)) ts
        (let ([r (get-min (rest ts))])
          (if (leq (node-root (first ts))
                   (node-root (first r)))
              ts (cons (first r)
                       (cons (first ts)
                             (rest r)))))))
  (let ([r (get-min ts)])
    (biheap-merge (reverse (node-chld (first r)))
                  (rest r) leq)))

; biheap size
(define (biheap-size ts)
  (foldl (λ (t acc)
           (+ acc
              (arithmetic-shift
               1 (node-rank t)))) 0 ts))

; ============= Tests =================
(require rackunit)

(check-equal?
 (biheap-min
  (foldr biheap-insert '() (range 100)))
 0)

(check-equal?
 (biheap-min
  (foldr
   (λ (x acc)
     (biheap-insert x acc >=))
   '() (range 100)) >=)
 99)

(check-equal?
 (biheap-min
  (biheap-rm-min
   (foldr
    (λ (x acc)
      (biheap-insert x acc >=))
    '() (range 100)) >=) >=)
 98)