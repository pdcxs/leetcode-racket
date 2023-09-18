#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

; robbing a hourse
; is the maximum of
; rob this hourse?
; or not.
(define (rob root)
  (if (not root) 0
      (let ([robbed
             (+ (tree-node-val root)
                (rob-child (tree-node-left root))
                (rob-child (tree-node-right root)))]
            [not-rob (+ (mem-rob (tree-node-left root))
                        (mem-rob (tree-node-right root)))])
        (max robbed not-rob))))

(define (mem-f f)
  (define h (make-hash))
  (λ (x)
    (if (hash-has-key? h x)
        (hash-ref h x)
        (let ([v (f x)])
          (hash-set! h x v)
          v))))

(define mem-rob (mem-f rob))

(define (rob-child root)
  (if (not root) 0
      (+ (mem-rob (tree-node-left root))
         (mem-rob (tree-node-right root)))))

; =========