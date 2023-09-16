#lang racket

; This problem gives following
; singly-linked list structure.
; To solve this problem, I have to use it.
; But in practical functional programming,
; we will usually use cons list.

(struct list-node
  (val next) #:mutable #:transparent)

(define (make-list-node [val 0])
  (list-node val #f))

; Main function
; Interesting implementation
; We add an optional parameter for main function
; with default value
(define (add-two-numbers l1 l2 (carry-in 0))
  (cond
    [(or l1 l2 (positive? carry-in))
     (define-values
       (carry-out val)
       (quotient/remainder
        (+
         (if l1 (list-node-val l1) 0)
         (if l2 (list-node-val l2) 0)
         carry-in) 10))
     (list-node val
                (add-two-numbers
                 (and l1 (list-node-next l1))
                 (and l2 (list-node-next l2))
                 carry-out))]
    [else #f]))

; ============= Test ======================
(require rackunit)

(define (number->list n)
  (define zero (char->integer #\0))
  (reverse
   (map (λ (x) (- (char->integer x) zero))
        (string->list
         (number->string n)))))

(define (list->list-node lst)
  (if (null? lst) #f
      (list-node (first lst)
                 (list->list-node (rest lst)))))

(define (list-node->list node)
  (if node
      (cons
       (list-node-val node)
       (list-node->list
        (list-node-next node)))
      '()))

(define (list->number lst)
  (second
   (foldl (λ (x acc)
            (list
             (* (first acc) 10)
             (+ (* x (first acc)) (second acc))))
          '(1 0) lst)))

(define inputs
  '((0 0)
    (321 123)
    (1 9999)
    (9999 1)
    (1234 5678)))

(for ([i inputs])
  (let ([n1 (first i)]
        [n2 (second i)])
    (check-equal?
     (list->number
      (list-node->list
       (add-two-numbers
        (list->list-node
         (number->list n1))
        (list->list-node
         (number->list n2)))))
     (+ n1 n2))))