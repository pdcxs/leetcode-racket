#lang racket

; Build a tree
; tree is a hash table
; (hash-ref tree i) is
; the child list of node i
(define (build-tree parents)
  (define (scan parents idx tree)
    (if (null? parents) tree
        (let* ([p (first parents)]
               [c (hash-ref tree p '())])
          (scan (rest parents) (add1 idx)
                (hash-set tree p (cons idx c))))))
  (scan (rest parents) 1 (hash)))

(define (find-one vec)
  (define n (vector-length vec))
  (let loop ([i 0])
    (cond
      [(>= i n) 0]
      [(= (vector-ref vec i) 1) i]
      [else (loop (add1 i))])))

; find least missed value in a set
(define (least-value s [init-value 1])
  (if (set-member? s init-value)
      (least-value s (add1 init-value))
      init-value))

(define (collect-nums node tree nums visited num-set)
  (if (set-member? visited node) num-set
      (let* ([child (hash-ref tree node '())]
             [ns (map
                  (λ (c)
                    (collect-nums
                     c tree nums visited (set)))
                  child)])
        (set-union (set-add num-set
                            (vector-ref nums node))
                   (foldl set-union (set) ns)))))

(define (smallest-missing-value-subtree parents nums)
  (define vec-p (list->vector parents))
  (define vec-n (list->vector nums))
  (define n (vector-length vec-p))

  (define tree (build-tree parents))
  
  (define node-1 (find-one vec-n))
  (define ans (make-vector n 1))
  (define (scan node num-set least-num visited)
    (unless (= node -1)
      (let* ([ns (collect-nums node tree vec-n
                               visited num-set)]
             [next-visited (set-add visited node)]
             [next-least (least-value
                          ns least-num)]
             [p (vector-ref vec-p node)])
        (vector-set! ans node next-least)
        (scan p ns next-least next-visited))))
  (scan node-1 (set) 1 (set))
  (vector->list ans))

; ========================= Test =================
(require rackunit)
(check-equal?
 (smallest-missing-value-subtree
  '(-1 0 0 2)
  '(1 2 3 4))
 '(5 1 1 1))
(check-equal?
 (smallest-missing-value-subtree
  '(-1 0 1 0 3 3)
  '(5 4 6 2 1 3))
 '(7 1 1 4 2 1))
(check-equal?
 (smallest-missing-value-subtree
  '(-1 2 3 0 2 4 1)
  '(2 3 4 5 6 7 8))
 '(1 1 1 1 1 1 1))