#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; robbing a hourse
; is the maximum of
; rob this hourse?
; or not.
(define (rob root)
  (if (not root) 0 ; for a null root, cannot rob anymore
      (let ([robbed ; how much can we rob when rob root node
             (+ (tree-node-val root) ; rob root
                ; rob children of left child
                (rob-child (tree-node-left root))
                ; rob children of right child
                (rob-child (tree-node-right root)))]
            ; how much can we rob when don't rob root node
            ; rob left child and rob right child
            [not-rob (+ (mem-rob (tree-node-left root))
                        (mem-rob (tree-node-right root)))])
        ; take the maximum of robbing and not robbing root
        (max robbed not-rob))))

; memoization
(define (mem f)
  (define h (make-hash))
  (λ (x)
    (if (hash-has-key? h x)
        (hash-ref h x)
        (let ([v (f x)])
          (hash-set! h x v)
          v))))

(define mem-rob (mem rob))

; Robs the left tree and right tree of root.
; Don't rob the root itself.
(define (rob-child root)
  (if (not root) 0
      (+ (mem-rob (tree-node-left root))
         (mem-rob (tree-node-right root)))))

; =========== Test ===============
(require rackunit)
(check-equal?
 (rob (tree-node
       3
       (tree-node 2 #f (tree-node 3 #f #f))
       (tree-node 3 #f (tree-node 1 #f #f))))
 7)

(check-equal?
 (rob (tree-node
       3
       (tree-node
        4 (tree-node 1 #f #f)
        (tree-node 3 #f #f))
       (tree-node 5 #f
                  (tree-node 1 #f #f))))
 9)