#lang racket

(require "./stream.rkt")

(provide
 make-dequeue
 dequeue-push-forward
 dequeue-push-backward
 dequeue-empty?
 dequeue-first
 dequeue-rest
 dequeue-last
 dequeue-most)

; Double Ended Queue (dequeue)
; Which has following structure:
; (list
;   front-length: number
;   front-stream: stream
;   rear-length: number
;   rear-stream: stream)
(define (make-dequeue [lst '()])
  (check
   (list
    (length lst)
    (list->stream lst)
    0 empty-stream)))

; Define the getters
(define dequeue-front-length first)
(define dequeue-front second)
(define dequeue-rear-length third)
(define dequeue-rear fourth)

; The maximum number of times longer
; one half of a dequeue is
; permitted to be relative to the other.
(define balance-fac 4)

; check function
; Ensure the balance of a dequeue
(define (check dequeue)
  (let ([fl (dequeue-front-length dequeue)]
        [fs (dequeue-front dequeue)]
        [rl (dequeue-rear-length dequeue)]
        [rs (dequeue-rear dequeue)])
    (cond
      [(> fl (add1 (* balance-fac rl)))
       (let* ([i (arithmetic-shift (+ fl rl) -1)]
              [j (- (+ fl rl) i)]
              [nfs (stream-take fs i)]
              [nrs (stream-append
                    rs
                    (stream-reverse (stream-drop fs i)))])
         (list i nfs j nrs))]
      [(> rl (add1 (* balance-fac fl)))
       (let* ([i (arithmetic-shift (+ fl rl) -1)]
              [j (- (+ fl rl) i)]
              [nfs (stream-append
                    fs
                    (stream-reverse (stream-drop rs j)))]
              [nrs (stream-take rs j)])
         (list i nfs j nrs))]
      [else dequeue])))

(define (dequeue-push-forward x dequeue)
  (check
   (list
    (add1 (dequeue-front-length dequeue))
    (stream-cons
     x (dequeue-front dequeue))
    (dequeue-rear-length dequeue)
    (dequeue-rear dequeue))))

(define (dequeue-push-backward x dequeue)
  (check
   (list
    (dequeue-front-length dequeue)
    (dequeue-front dequeue)
    (add1 (dequeue-rear-length dequeue))
    (stream-cons
     x (dequeue-rear dequeue)))))

(define (dequeue-empty? dequeue)
  (and
   (= (dequeue-front-length dequeue) 0)
   (= (dequeue-rear-length dequeue) 0)))

(define (dequeue-first dequeue)
  (cond [(dequeue-empty? dequeue)
         (error "Error in dequeue-first: empty dequeue.")]
        [(= (dequeue-front-length dequeue) 0)
         ; The only possible case here
         ; is the length of rear stream
         ; is one.
         (stream-first (dequeue-rear dequeue))]
        [else
         (stream-first (dequeue-front dequeue))]))

(define (dequeue-rest dequeue)
  (cond [(dequeue-empty? dequeue)
         (error "Error in dequeue-rest: empty dequeue.")]
        [(zero? (dequeue-front-length dequeue))
         ; rest of a singleton dequeue
         ; is an empty dequeue
         (make-dequeue)]
        [else
         (check
          (list
           (sub1 (dequeue-front-length dequeue))
           (stream-rest (dequeue-front dequeue))
           (dequeue-rear-length dequeue)
           (dequeue-rear dequeue)))]))

(define (dequeue-last dequeue)
  (cond [(dequeue-empty? dequeue)
         (error "Error in dequeue-last: empty dequeue.")]
        [(= (dequeue-rear-length dequeue) 0)
         ; The only possible case here is
         ; the length of dequeue front is one.
         (stream-first (dequeue-front dequeue))]
        [else
         (stream-first (dequeue-rear dequeue))]))

(define (dequeue-most dequeue)
  (cond [(dequeue-empty? dequeue)
         (error "Error in dequeue-most: empty dequeue.")]
        [(= (dequeue-rear-length dequeue) 0)
         ; most of a singleton is empty
         (make-dequeue)]
        [else
         (list
          (dequeue-front-length dequeue)
          (dequeue-front dequeue)
          (sub1 (dequeue-rear-length dequeue))
          (stream-rest (dequeue-rear dequeue)))]))

; ================== Test ======================
(require rackunit)

; Test 1
(check-equal?
 (dequeue-first
  (dequeue-push-backward
   2 (dequeue-push-forward 1 (make-dequeue))))
 1)
; Test 2
(check-equal?
 (dequeue-last
  (dequeue-push-backward
   2 (dequeue-push-forward 1 (make-dequeue))))
 2)
; Test 3
(check-equal?
 (dequeue-last
  (foldl
   (λ (x acc) (dequeue-push-backward x acc))
   (foldl (λ (x acc) (dequeue-push-forward x acc))
          (make-dequeue) (range 100))
   (range 101 200))) 199)
; Test 4
(check-equal?
 (dequeue-last
  (foldl
   (λ (x acc) (dequeue-push-forward x acc))
   (foldl (λ (x acc) (dequeue-push-forward x acc))
          (make-dequeue) (range 100))
   (range 101 200))) 0)