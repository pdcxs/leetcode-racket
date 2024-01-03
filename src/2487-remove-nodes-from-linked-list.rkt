#lang racket

(define (remove-nodes head)
  (if (not head) #f
      (let* ([nxt (list-node-next head)]
             [r (remove-nodes nxt)]
             [v (list-node-val head)])
        (cond
          [(not r) (make-list-node v)]
          [(>= v (list-node-val r))
           (list-node v r)]
          [else r]))))

; ============== Test ================
(struct list-node
  (val next) #:mutable #:transparent)

(define (make-list-node [val 0])
  (list-node val #f))

(define (list->struct lst)
  (if (null? lst) #f
      (list-node
       (first lst)
       (list->struct (rest lst)))))

(require rackunit)
(check-equal?
 (remove-nodes (list->struct '(5 2 13 3 8)))
 (list->struct '(13 8)))

(check-equal?
 (remove-nodes (list->struct '(1 1 1 1)))
 (list->struct '(1 1 1 1)))