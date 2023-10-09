#lang racket

; A graph is needed.
; A graph is a hash table
; graph[i] is the set of neighbors of node i
; This is a trade-off method,
; I use mutable vector to store the degree.
; It just works.

(require "../libs/dequeue.rkt")

(define (build-graph n edges)
  (define degree (make-vector n))
  (define (scan edges graph)
    (if
     (null? edges) (values graph degree)
     (let* ([n1 (first (first edges))]
            [n2 (second (first edges))]
            [nb1 (set-add (hash-ref graph n1 (set)) n2)]
            [nb2 (set-add (hash-ref graph n2 (set)) n1)]
            [hash-n1 (hash-set graph n1 nb1)]
            [hash-n2 (hash-set hash-n1 n2 nb2)])
       (vector-add1! degree n1)
       (vector-add1! degree n2)
       (scan (rest edges) hash-n2))))
  (scan edges (hash)))

(define (vector-add1! vec index)
  (vector-set!
   vec index (add1 (vector-ref vec index))))

(define (vector-sub1! vec index)
  (vector-set!
   vec index (sub1 (vector-ref vec index))))

(define (is-leaf? degree node)
  (= (vector-ref degree node) 1))

(define (is-zero-leaf? degree coins node)
  (and (zero? (vector-ref coins node))
       (is-leaf? degree node)))

; remove leaf nodes with zero coin
(define (remove-zero-leaf! n graph coins degree)
  ; we first get all leaves with zero coin
  (define (get-zero-leaves queue i)
    (if (= i n) queue
        (if (is-zero-leaf? degree coins i)
            (get-zero-leaves
             (dequeue-push-backward i queue)
             (add1 i))
            (get-zero-leaves queue (add1 i)))))
  ; store zero-leaves in a dequeue
  (define zero-leaves (get-zero-leaves (make-dequeue) 0))
  ; remove them, and remove the new zero-leaves
  (define (delete-leaf! queue remain)
    (if (dequeue-empty? queue)
        ; return two variable: number of remaining nodes
        ; and current degrees of each node
        remain
        (let* ([u (dequeue-first queue)])
          (vector-sub1! degree u)
          ; sub degree of u's neighbors
          (let ([q (remove-nb!
                    ; dequeue-rest means we've already
                    ; deleted node u.
                    u (dequeue-rest queue))])
            (delete-leaf! q (sub1 remain))))))
  ; modify degrees of the neighbors of a node
  ; add the new zero-leaves into the queue
  (define (remove-nb! node queue)
    (define (scan nb queue)
      (if (set-empty? nb) queue
          (let ([v (set-first nb)])
            (vector-sub1! degree v)
            (if (is-zero-leaf? degree coins v)
                (scan (set-rest nb)
                      (dequeue-push-backward v queue))
                (scan (set-rest nb) queue)))))
    (scan (hash-ref graph node (set)) queue))
  (delete-leaf! zero-leaves n))

; delete all leaf nodes
(define (delete-leaves! n graph degree remain)
  ; get all leaves
  (define (get-leaves i leaves)
    (cond
      [(= i n) leaves]
      [(is-leaf? degree i)
       (get-leaves (add1 i)
                   (cons i leaves))]
      [else
       (get-leaves (add1 i) leaves)]))
  ; delete current leaves
  (define (delete-node! leaves remain)
    (if (null? leaves)
        remain
        (let ([node (first leaves)])
          (if (is-leaf? degree node)
              (begin
                (vector-sub1! degree node)
                (delete-nb! node)
                (delete-node! (rest leaves)
                              (sub1 remain)))
              (delete-node! (rest leaves) remain)))))
  (define (delete-nb! node)
    (define (scan nb)
      (unless (set-empty? nb)
        (vector-sub1! degree (set-first nb))
        (scan (set-rest nb))))
    (scan (hash-ref graph node)))
  (delete-node!
   (get-leaves 0 '()) remain))

; Main function
(define (collect-the-coins coins edges)
  (let* ([coins-vec (list->vector coins)]
         [n (vector-length coins-vec)])
    (let-values
        ([(graph degree) (build-graph n edges)])
      (let*
          ([r1 (remove-zero-leaf!
                n graph coins-vec degree)]
           [r2 (delete-leaves! n graph degree r1)]
           [r3 (delete-leaves! n graph degree r2)])
        (if (zero? r3) 0
            (* 2 (sub1 r3)))))))

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

(check-equal?
 (collect-the-coins
  '(1 1 1 0 0 0 0 0 1 0 0 1 1 0 1 1 0 0 1)
  '((0 1) (1 2) (2 3) (1 4) (4 5) (5 6) (6 7)
          (3 8) (6 9) (7 10) (10 11) (10 12)
          (7 13) (12 14) (13 15) (14 16) (15 17) (10 18)))
 12)
