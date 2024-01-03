#lang racket

(provide
 adj-verts
 build-graph
 outdegree
 indegree
 top-sort
 preorder
 preorder-forest
 postorder
 postorder-forest
 dfs
 print-tree)

(require "./stream.rkt")

; A Graph is a hash table G
; where G[v] is a set of adjacent
; vertices of node v.

; Get adjacent vertices of node v.
(define (adj-verts graph v)
  (hash-ref graph v '()))

; Build graph from list of edges.
; An edge is (list v1 v2),
; which represents an edge from
; node v1 to node v2.
(define (build-graph edges [graph (hash)])
  (match edges
    ['() graph]
    [(cons (list v1 v2) rest-es)
     (let* ([adj (adj-verts graph v1)]
            [next-adj (set-add adj v2)])
       (build-graph
        rest-es (hash-set graph v1 next-adj)))]
    [else
     (error 'build-graph
            "Unsupported edge form: ~a."
            (first edges))]))

; Get outdegree of a vertex.
(define (outdegree graph v)
  (set-count (adj-verts graph v)))

; Get indegree of a vertex
(define (indegree graph v)
  (define (scan pos s)
    (if pos
        (let ([next (hash-iterate-next
                     graph pos)]
              [adj (hash-iterate-value
                    graph pos)])
          (if (set-member? adj v)
              (scan next (add1 s))
              (scan next s)))
        s))
  (scan (hash-iterate-first graph) 0))

; A Tree is a data structure:
; (list 'node root forest)
; And a forest is a stream of trees
(define (make-tree root [children empty-stream])
  (list 'node root children))

(define get-root second)
(define get-child third)

; Show a tree
(define (print-tree node)
  (let ([r (get-root node)]
        [c (get-child node)])
    (let ([cs (stream->list
               (stream-map print-tree
                           c))])
      (make-tree r cs))))

; Show a nested stream
;(define (print-stream s)
;  (cond
;    [(stream-empty? s) '()]
;    [(stream? (stream-first s))
;     (cons (print-stream (stream-first s))
;           (print-stream (stream-rest s)))]
;    [else
;     (cons (stream-first s)
;           (print-stream (stream-rest s)))]))

; dfs: A spanning forest of the part of
; the graph reachable from the listed
; vertices, obtained from a depth-first
; search of the graph starting at each
; of the listed vertices in order.
; To reduce the memory,
; here vertices is a stream.
(define (dfs graph vertices)
  (define visited (mutable-set))
  (define (scan verts)
    (if
     (stream-empty? verts) empty-stream
     (let ([v (stream-first verts)]
           [vs (stream-rest verts)])
       (if (set-member? visited v)
           (scan vs)
           (begin
             (set-add! visited v)
             (let ([as (scan (adj-verts graph v))]
                   [bs (scan vs)])
               (stream-cons
                (make-tree v as) bs)))))))
  (scan vertices))

; preorder of a tree
(define (preorder tree)
  (match tree
    [(list 'node root children)
     (stream-cons root
                  (preorder-forest children))]
    [else
     (error 'preorder
            "Unsupported tree format: ~a"
            tree)]))

; preorder of a forest
(define (preorder-forest ts)
  (stream-fold
   stream-append
   empty-stream
   (stream-map preorder ts)))

; postorder of a tree
(define (postorder tree)
  (match tree
    [(list 'node root children)
     (stream-append
      (postorder-forest children)
      (stream root))]
    [else
     (error 'postorder
            "Unsupported tree format: ~a"
            tree)]))

; postorder of a forest
(define (postorder-forest ts)
  (stream-fold
   stream-append
   empty-stream
   (stream-map postorder ts)))

; Topology Sort
(define (top-sort graph verts)
  (let ([forest (dfs graph verts)])
    (stream-reverse (postorder-forest forest))))