#lang racket

(provide
 make-queue
 queue-empty?
 queue-first
 queue-rest
 enqueue
 queue-length
 queue->list)

(require "./stream.rkt")

; A queue is the following data:
; (list front front-stream front-length
;             rear rear-length)
; The types of the argument is:
; front: list
; front-stream: stream
; front-length: number
; rear: list
; rear-length: number
; Only the front-stream is lazy

(define (make-queue)
  (list '() empty-stream 0 '() 0))

; Define the getters
(define queue-front first)
(define queue-front-stream second)
(define queue-front-length third)
(define queue-rear fourth)
(define queue-rear-length fifth)

; We will gaurantee that
; front of the queue is always not empty
(define (queue-empty? queue)
  (null? (queue-front queue)))

; Here is the guarantee function:
(define (check-front queue)
  (if (null? (queue-front queue))
      (cons (stream->list
             (queue-front-stream queue))
            (rest queue))
      queue))

(define (check-rear queue)
  (if (<=
       (queue-rear-length queue)
       (queue-front-length queue))
      queue
      (let ([f (stream->list
                (queue-front-stream queue))])
        (list
         f
         (stream-lazy
          (list->stream
           (append
            f (reverse (queue-rear queue)))))
         (+ (queue-front-length queue)
            (queue-rear-length queue))
         '() 0))))

(define (check queue)
  (check-front (check-rear queue)))

; Queue First
; takes the first element of a queue
(define (queue-first queue)
  (if (queue-empty? queue)
      (error "queue-first on empty queue.")
      (first (queue-front queue))))

; Queue Rest
; Returns the rest of a queue.
(define (queue-rest queue)
  (if (queue-empty? queue)
      (error "queue-rest on empty queue.")
      (check
       (list
        (rest (queue-front queue))
        (stream-rest (queue-front-stream queue))
        (sub1 (queue-front-length queue))
        (queue-rear queue)
        (queue-rear-length queue)))))

; Enqueue
; Push an element into a queue
(define (enqueue queue a)
  (check
   (list
    (queue-front queue)
    (queue-front-stream queue)
    (queue-front-length queue)
    (cons a (queue-rear queue))
    (add1 (queue-rear-length queue)))))

; Be careful
; This could be expensive
(define (queue->list queue)
  (append
   (queue-front queue)
   (reverse (queue-rear queue))))

(define (queue-length queue)
  (+ (queue-front-length queue)
     (queue-rear-length queue)))
