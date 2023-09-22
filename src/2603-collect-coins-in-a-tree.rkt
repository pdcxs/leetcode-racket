#lang racket

; A graph is needed.
; A graph is a hash-table
; key is node
; value is its neighbors
; The hash-table based method is too slow,
; so I provide a new method based on vector.
; I also tried rlist,
; but it is even slower than the hash-table one.

(define (build-graph edges)
  (define (scan edges graph)
    (if
     (null? edges) graph
     (let* ([n1 (first (first edges))]
            [n2 (second (first edges))]
            [nb1 (cons n2 (hash-ref graph n1 '()))]
            [nb2 (cons n1 (hash-ref graph n2 '()))])
       (scan (rest edges)
             (hash-set
              (hash-set graph n1 nb1)
              n2 nb2)))))
  (scan edges (hash)))

(define (single-or-empty? lst)
  (or
   (null? lst)
   (null? (rest lst))))

; remove leaf nodes with zero coin
(define (remove-zero-leaf graph coins)
  ; returns value of remove-once-time
  ; the nodes need to be removed
  (define (remove-once-time graph)
    (let loop ([i (hash-iterate-first graph)]
               [removed '()])
      (if i ; we can still iterate
          (let ([nb (hash-iterate-value graph i)])
            (if (and
                 ; its neighbors is a singleton list
                 (single-or-empty? (hash-iterate-value graph i))
                 ; it has no coin
                 (zero?
                  (vector-ref
                   coins
                   (hash-iterate-key graph i))))
                ; remove this node
                (loop (hash-iterate-next graph i)
                      (cons (hash-iterate-key graph i)
                            removed))
                (loop (hash-iterate-next graph i)
                      removed)))
          removed)))
  (let ([rn (remove-once-time graph)])
    (if (null? rn)
        graph
        (remove-zero-leaf
         (remove-nodes graph rn)
         coins))))

; remove a node in a graph
(define (rm graph node)
  (define rg (hash-remove graph node))
  (let loop ([g rg]
             [i (hash-iterate-first rg)])
    (if
     i
     (loop
      (hash-update
       g (hash-iterate-key g i)
       (λ (nb) (filter (λ (n) (not (= n node))) nb)))
      (hash-iterate-next g i))
     g)))
; remove a list of nodes
(define (remove-nodes graph nodes)
  (if (null? nodes)
      graph
      (remove-nodes
       (rm graph (first nodes))
       (rest nodes))))

; delete all leaf nodes
(define (delete-leaf graph)
  (define (get-leaf graph)
    (let loop ([i (hash-iterate-first graph)]
               [r '()])
      (if i
          (loop
           (hash-iterate-next graph i)
           (let ([nb (hash-iterate-value graph i)])
             (if (single-or-empty? nb)
                 (cons (hash-iterate-key graph i) r) r)))
          r)))
  (remove-nodes graph (get-leaf graph)))

; Main function
(define (collect-the-coins coins edges)
  (define (edge-count graph)
    (let loop ([i (hash-iterate-first graph)]
               [count 0])
      (if i
          (loop (hash-iterate-next graph i)
                (+ count
                   (length
                    (hash-iterate-value graph i))))
          count)))
  (let ([graph
         (remove-zero-leaf
          (build-graph edges)
          (list->vector coins))])
    (edge-count
     (delete-leaf
      (delete-leaf
       graph)))))

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