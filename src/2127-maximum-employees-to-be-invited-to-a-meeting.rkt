#lang racket

(require data/queue)

; Same Method as C++ Solution
; But time-out for racket.

(define (build-in-degree favorite n)
  (define in-degree (make-vector n 0))
  (define (scan lst)
    (if (null? lst) in-degree
        (let ([f (first lst)])
          (vector-set!
           in-degree f
           (add1 (vector-ref
                  in-degree f)))
          (scan (rest lst)))))
  (scan favorite))

(define (put-zero-node-to! in-degree queue)
  (define n (vector-length in-degree))
  (let loop ([i 0])
    (if (>= i n) queue
        (let ([u (vector-ref in-degree i)])
          (begin
            (when (zero? u)
              (enqueue! queue i))
            (loop (add1 i)))))))

(define (top-sort! fav in-degree n)
  (define visited (make-vector n #f))
  (define dist (make-vector n 1))
  (define queue (make-queue))

  (define (scan queue)
    (unless (queue-empty? queue)
      (let* ([u (dequeue! queue)]
             [v (vector-ref fav u)]
             [du (vector-ref dist u)]
             [dv (vector-ref dist v)]
             [iv (sub1 (vector-ref in-degree v))])
        (vector-set! visited u #t)
        (vector-set! dist v
                     (max dv (add1 du)))
        (vector-set! in-degree v iv)
        (when (zero? iv)
          (enqueue! queue v)))
      (scan queue)))

  (put-zero-node-to! in-degree queue)
  (scan queue)
  (values visited dist))

(define (maximum-invitations favorite)
  (define fav (list->vector favorite))
  (define n (vector-length fav))
  (define in-degree (build-in-degree favorite n))
  (define-values
    (visited dist)
    (top-sort! fav in-degree n))
  (define (count-ring idx)
    (define (scan u cnt)
      (if (= u idx) cnt
          (scan (vector-ref fav u)
                (add1 cnt))))
    (scan (vector-ref fav idx) 1))
  (define (scan idx ring total)
    (cond
      [(>= idx n) (max ring total)]
      [(not (vector-ref visited idx))
       (let ([node (vector-ref fav idx)])
         (if (= (vector-ref fav node) idx)
             (begin
               (vector-set! visited idx #t)
               (vector-set! visited node #t)
               (scan (add1 idx) ring
                     (+ total
                        (vector-ref dist idx)
                        (vector-ref dist node))))
             (let ([r (count-ring idx)])
               (scan (add1 idx)
                     (max ring r) total))))]
      [else
       (scan (add1 idx) ring total)]))
  (scan 0 0 0))

; =================== Test ==================
(require rackunit)
(check-equal?
 (maximum-invitations '(2 2 1 2))
 3)
(check-equal?
 (maximum-invitations '(1 2 0))
 3)
(check-equal?
 (maximum-invitations '(3 0 1 4 1))
 4)