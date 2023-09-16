#lang racket
(require "./stream.rkt")

(provide
 make-queue
 queue-empty?
 queue-first
 queue-rest
 enqueue
 queue-length
 queue->list)

; A Banker's Implementation of Queue

; A Queue has following structure:
; (list front-length
;       front-stream
;       rear-length
;       rear-stream)
(define (make-queue [lst '()])
  (if (null? lst)
      (list 0 empty-stream 0 empty-stream)
      (list (length lst)
            (list->stream lst)
            0
            empty-stream)))

; Define the Getters
(define queue-front-length first)
(define queue-front second)
(define queue-rear-length third)
(define queue-rear fourth)

; We will ensure the front queue is
; not empty for any non-empty queue.
(define (queue-empty? queue)
  (stream-empty?
   (queue-front queue)))

(define (queue-first queue)
  (cond
    [(queue-empty? queue)
     (error "Error in queue-first: empty queue")]
    [(not (stream-empty?
           (queue-front queue)))
     (stream-first (queue-front queue))]
    [else
     (stream-last (queue-rear queue))]))

(define (queue-rest queue)
  (if (queue-empty? queue)
      (error "Error in queue-rest: empty queue")
      (check
       (list
        (sub1 (queue-front-length queue))
        (stream-rest (queue-front queue))
        (queue-rear-length queue)
        (queue-rear queue)))))

(define (enqueue queue a)
  (if (queue-empty? queue)
      (list
       1
       (stream-lazy
        (stream-cons a empty-stream))
       (queue-rear-length queue)
       (queue-rear queue))
      (check
       (list
        (queue-front-length queue)
        (queue-front queue)
        (add1 (queue-rear-length queue))
        (stream-cons a (queue-rear queue))))))

(define (queue-length queue)
  (+ (queue-front-length queue)
     (queue-rear-length queue)))

(define (check queue)
  (if (<=
       (queue-rear-length queue)
       (queue-front-length queue))
      queue
      (list
       (queue-length queue)
       (stream-lazy
        (stream-append
         (queue-front queue)
         (stream-reverse
          (queue-rear queue))))
       0 empty-stream)))

(define (queue->list queue)
  (stream->list
   (stream-append
    (queue-front queue)
    (stream-reverse
     (queue-rear queue)))))


; ================= Tests ====================
; To prove the queue is lazy
; try this:
; (define n 1000000)
; (define queue (list 1 (list->stream '(1)) n (in-range n)))
; (time (queue-first queue))
; (time (check queue))
; (time (queue-first (queue-rest queue)))
; only take the head will not take long time
; but the second element taking will.

(require rackunit)
(check-equal?
 (queue-empty? (make-queue)) #true)
(check-equal?
 (queue-first (enqueue (enqueue (make-queue) 1) 2)) 1)