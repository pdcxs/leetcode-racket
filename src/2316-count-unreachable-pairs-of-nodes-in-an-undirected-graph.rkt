#lang racket

; We need a Disjoint-set data structure.
; parent: vector, parent[i] is the
;         parent node of node i.
; rank: depth of each tree, also a vector.
; size: number of nodes in each tree.
(define-struct dset [parent rank size])

; Initialize a Disjoint-set
(define (init-dset n)
  ; Parents of each nodes are themselves.
  (make-dset
   (build-vector n identity)
   (make-vector n 1)
   (make-vector n 1)))

; Interesting: find operation is mutable.
; Because we will change the parent of each node
; to make the find operation faster. 
(define (find! dset i)
  (let ([p (vector-ref (dset-parent dset) i)])
    (if (= p i) i
        (let ([r (find! dset p)])
          ; set parent of node i to the root.
          (vector-set! (dset-parent dset) i r)
          r))))

; Merge two nodes
(define (merge! dset i j)
  (let ([p1 (find! dset i)]
        [p2 (find! dset j)]
        [ps (dset-parent dset)]
        [rk (dset-rank dset)]
        [sz (dset-size dset)])
    (if (<= (vector-ref rk p1)
            (vector-ref rk p2))
        (vector-set! ps p1 p2)
        (vector-set! ps p2 p1))
    ; If two tree has same depth
    ; we need to increase the depth
    (when (and (= (vector-ref rk p1)
                  (vector-ref rk p2))
               (not (= p1 p2)))
      (vector-set! rk p2
                   (add1 (vector-ref rk p2))))
    ; Update the size
    (unless (= p1 p2)
      (let ([s1 (vector-ref sz p1)]
            [s2 (vector-ref sz p2)])
        (vector-set! sz p1 (+ s1 s2))
        (vector-set! sz p2 (+ s1 s2))))))

(define (count-pairs n edges)
  (define dset (init-dset n))
  (let loop ([e edges])
    (if (null? e)
        (count-dset-pair n dset)
        (begin
          (merge! dset (first (first e))
                  (second (first e)))
          (loop (rest e))))))

(define (count-dset-pair n dset)
  (define ps (dset-parent dset))
  (define sz (dset-size dset))
  (let loop ([s 0]
             [i 0])
    (if (= i n) (/ s 2)
        (loop (+ s (- n (vector-ref
                         sz (find! dset i))))
              (add1 i)))))

; ============== Test =================
(require rackunit)

(check-equal?
 (count-pairs 3 '((0 1) (0 2) (1 2)))
 0)

(check-equal?
 (count-pairs 7 '((0 2) (0 5) (2 4) (1 6) (5 4)))
 14)

(check-equal?
 (count-pairs 11 '((5 0) (1 0) (10 7) (9 8) (7 2) (1 3) (0 2) (8 5) (4 6) (4 2)))
 0)