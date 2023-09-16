#lang racket

(provide
 stream-reverse
 list->stream
 stream-last
 stream-singleton?
 stream-zipwith
 stream-drop)

(define (stream-reverse s)
  (define (reverse s r)
    (if (stream-empty? s) r
        (reverse (stream-rest s)
                 (stream-cons
                  (stream-first s) r))))
  (stream-lazy (reverse s empty-stream)))

(define (list->stream lst)
  (if (null? lst)
      empty-stream
      (stream-cons
       (car lst)
       (list->stream (cdr lst)))))

(define (stream-last s)
  (cond [(stream-empty? s)
         "Error in stream-last: empty stream"]
        [(stream-empty?
          (stream-rest s))
         (stream-first s)]
        [else
         (stream-last (stream-rest s))]))

(define (stream-singleton? s)
  (and
   (not (stream-empty? s))
   (stream-empty? (stream-rest s))))

(define (stream-zipwith f s1 s2)
  (if (or (stream-empty? s1)
          (stream-empty? s2))
      (empty-stream)
      (stream-cons
       (f (stream-first s1)
          (stream-first s2))
       (stream-zipwith
        f
        (stream-rest s1)
        (stream-rest s2)))))

(define (stream-drop f i)
  (stream-lazy
   (cond [(<= i 0) f]
         [(stream-empty? f) f]
         [else (stream-drop
                (stream-rest f) (sub1 i))])))