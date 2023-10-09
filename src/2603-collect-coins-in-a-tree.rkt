#lang racket

; A graph is needed.
; A graph is a hash table
; graph[i] is the set of neighbors of node i
; This is a purely functional method,
; but it's timeout.
; I also provide an ugly sucessful method.
; See: 2603-collect-coins-in-a-tree - mutable.rkt

(require "../libs/dequeue.rkt")

(define (build-graph n edges)
  (define (scan edges graph degree)
    (if
     (null? edges) (values graph degree)
     (let* ([n1 (first (first edges))]
            [n2 (second (first edges))]
            [nb1 (set-add (hash-ref graph n1 (set)) n2)]
            [nb2 (set-add (hash-ref graph n2 (set)) n1)]
            [degree1 (list-add1 degree n1)]
            [degree2 (list-add1 degree1 n2)]
            [hash-n1 (hash-set graph n1 nb1)]
            [hash-n2 (hash-set hash-n1 n2 nb2)])
       (scan (rest edges) hash-n2 degree2))))
  (scan edges (hash) (build-list n (λ (i) 0))))

(define (list-add1 lst index)
  (if (= index 0)
      (cons (add1 (first lst)) (rest lst))
      (cons (first lst)
            (list-add1 (rest lst) (sub1 index)))))

(define (list-sub1 lst index)
  (if (= index 0)
      (cons (sub1 (first lst)) (rest lst))
      (cons (first lst)
            (list-sub1 (rest lst) (sub1 index)))))

(define (is-leaf? degree node)
  (= (list-ref degree node) 1))

(define (is-zero-leaf? degree coins node)
  (and (zero? (vector-ref coins node))
       (is-leaf? degree node)))

; remove leaf nodes with zero coin
(define (remove-zero-leaf n graph coins degree)
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
  (define (delete-leaf queue remain degree)
    (if (dequeue-empty? queue)
        ; return two variable: number of remaining nodes
        ; and current degrees of each node
        (values remain degree)
        (let* ([u (dequeue-first queue)]
               [du (list-sub1 degree u)])
          ; sub degree of u's neighbors
          (let-values ([(q d)
                        (remove-nb
                         ; dequeue-rest means we've already
                         ; deleted node u.
                         u (dequeue-rest queue) du)])
            (delete-leaf q (sub1 remain) d)))))
  ; modify degrees of the neighbors of a node
  ; add the new zero-leaves into the queue
  (define (remove-nb node queue degree)
    (define (scan nb queue degree)
      (if (set-empty? nb) (values queue degree)
          (let* ([v (set-first nb)]
                 [dv (list-sub1 degree v)])
            (if (is-zero-leaf? dv coins v)
                (scan (set-rest nb)
                      (dequeue-push-backward v queue) dv)
                (scan (set-rest nb) queue dv)))))
    (scan (hash-ref graph node (set)) queue degree))
  (delete-leaf zero-leaves n degree))

; delete all leaf nodes
(define (delete-leaves n graph degree remain)
  ; get all leaves
  (define (get-leaves i degree leaves)
    (cond
      [(null? degree) leaves]
      [(= (first degree) 1)
       (get-leaves (add1 i) (rest degree)
                   (cons i leaves))]
      [else
       (get-leaves (add1 i) (rest degree) leaves)]))
  ; delete current leaves
  (define (delete-node leaves degree remain)
    (if (null? leaves)
        (values degree remain)
        (let ([node (first leaves)])
          (if (is-leaf? degree node)
              (delete-node (rest leaves)
                           (delete-nb
                            node (list-sub1 degree node))
                           (sub1 remain))
              (delete-node (rest leaves) degree remain)))))
  (define (delete-nb node degree)
    (define (scan nb degree)
      (if (set-empty? nb) degree
          (scan
           (set-rest nb)
           (list-sub1 degree (set-first nb)))))
    (scan (hash-ref graph node) degree))
  (delete-node
   (get-leaves 0 degree '())
   degree remain))

; Main function
(define (collect-the-coins coins edges)
  (let* ([coins-vec (list->vector coins)]
         [n (vector-length coins-vec)])
    (let*-values
        ([(graph degree) (build-graph n edges)]
         [(remain d)
          (remove-zero-leaf n graph coins-vec degree)]
         [(d1 r1) (delete-leaves n graph d remain)]
         [(d2 r2) (delete-leaves n graph d1 r1)])
      (if (zero? r2) 0
          (* 2 (sub1 r2))))))

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
