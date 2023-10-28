#lang racket

(provide
 make-biheap
 biheap-insert
 biheap-merge
 biheap-min
 biheap-rm-min
 biheap-size
 biheap-empty?
 list->biheap
 biheap->list)

; Binomial Heap (biheap)
; Also known as Priority Queue

; biheap is the following constructor:
; (cons
;  (list of trees)
;  (leq compare function))

; biheap constructor
; First is biheap contents
; Second is the compare function
(define (make-biheap [leq <=])
  (cons '() leq))

(define (list->biheap lst [leq <=])
  (foldl biheap-insert (make-biheap leq) lst))

(define (biheap->list heap)
  (if (biheap-empty? heap) '()
      (cons (biheap-min heap)
            (biheap->list (biheap-rm-min heap)))))

; Define biheap getters
(define biheap-content car)
(define biheap-leq cdr)

(define (biheap-empty? bihp)
  (null? (biheap-content bihp)))

; get the rest of a bihp
(define (biheap-rest bihp)
  (cons (rest (biheap-content bihp))
        (biheap-leq bihp)))

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
(define (link tree1 tree2 leq)
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
(define (insert-tree t ts leq)
  (if (null? ts)
      (list t)
      (let ([h (first ts)])
        (if (< (node-rank t) (node-rank h))
            (cons t ts)
            (insert-tree (link t h leq)
                         (rest ts) leq)))))

; insert a value into a binomial heap
(define (biheap-insert x bihp)
  (let ([ts (biheap-content bihp)]
        [leq (biheap-leq bihp)])
    (cons
     (insert-tree
      (make-node 0 x '()) ts leq)
     leq)))

; merge two biheaps
(define (biheap-merge bihp1 bihp2)
  (define ts1 (biheap-content bihp1))
  (define ts2 (biheap-content bihp2))
  (define leq (biheap-leq bihp1))
  (cond [(null? ts1) bihp2]
        [(null? ts2) bihp1]
        [(< (node-rank (first ts1))
            (node-rank (first ts2)))
         (cons
          (cons (first ts1)
                (biheap-content
                 (biheap-merge
                  (biheap-rest bihp1) bihp2)))
          leq)]
        [(< (node-rank (first ts2))
            (node-rank (first ts1)))
         (cons
          (cons
           (first ts2)
           (biheap-content
            (biheap-merge
             bihp1 (biheap-rest bihp2))))
          leq)]
        [else
         (cons
          (insert-tree
           (link (first ts1)
                 (first ts2) leq)
           (biheap-content
            (biheap-merge
             (biheap-rest bihp1)
             (biheap-rest bihp2))) leq)
          leq)]))

; find minimum element in a biheap
(define (biheap-min bihp)
  (define ts (biheap-content bihp))
  (define leq (biheap-leq bihp))
  (if (null? ts)
      (error "Error in biheap-min: Empty biheap.")
      (if (null? (rest ts)) ; only has one tree
          (node-root (first ts))
          (let ([x (node-root (first ts))]
                [y (biheap-min (biheap-rest bihp))])
            (if (leq x y) x y)))))

; delete minimum element in a biheap
(define (biheap-rm-min bihp)
  (define ts (biheap-content bihp))
  (define leq (biheap-leq bihp))
  ; get-min returns a pair:
  ; (cons t ts)
  ; where t is the tree has minimum element
  ; ts is the trees without t.
  (define (get-min ts)
    (if
     (null? ts)
     (error "Error in biheap-rm-min: Empty biheap.")
     (if (null? (rest ts))
         ; When ts is singleton
         ; (cons (first ts) '()) is just ts
         ts
         (let ([r (get-min (rest ts))])
           ; for clarity,
           ; I use `first` on list and `car` on pair.
           ; But they are actually the same function.
           (if (leq (node-root (first ts))
                    (node-root (car r)))
               ; (cons (first ts) (rest ts)) is just ts
               ts
               (cons (car r)
                     ; Second of the pair is a list
                     (cons (first ts)
                           (cdr r))))))))
  (let ([r (get-min ts)])
    (biheap-merge
     (cons (reverse (node-chld (car r))) leq)
     (cons (rest r) leq))))

; biheap size
(define (biheap-size bihp)
  (foldl (λ (t acc)
           (+ acc
              (arithmetic-shift
               1 (node-rank t))))
         0 (biheap-content bihp)))

; ============= Tests =================
(require rackunit)

(check-equal?
 (biheap-min
  (foldr biheap-insert (make-biheap) (range 100)))
 0)

(check-equal?
 (biheap-min
  (foldr
   (λ (x acc)
     (biheap-insert x acc))
   (make-biheap >=) (range 100)))
 99)

(check-equal?
 (biheap-min
  (biheap-merge
   (foldr biheap-insert (make-biheap)
          (range 50 100))
   (foldr biheap-insert (make-biheap)
          (range 50))))
 0)

(check-equal?
 (biheap-min
  (biheap-merge
   (foldr biheap-insert (make-biheap >=)
          (range 50 100))
   (foldr biheap-insert (make-biheap >=)
          (range 50))))
 99)

(check-equal?
 (biheap-min
  (biheap-rm-min
   (foldr
    (λ (x acc)
      (biheap-insert x acc))
    (make-biheap >=) (range 100))))
 98)