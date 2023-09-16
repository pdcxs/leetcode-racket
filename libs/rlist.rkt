#lang racket

; Binary Random Access List (rlist)

(provide
 rlist-cons
 rlist-lookup
 rlist-update)

; First we need a binary tree
; a binary tree can be
; a leaf: (list 'leaf value)
; or a node: (list 'node size left-tree right-tree)

; type related functions
(define (leaf? tree)
  (eq? (first tree) 'leaf))
(define (node? tree)
  (eq? (first tree) 'node))
(define leaf-val second)
(define node-size second)
(define node-left third)
(define node-right fourth)
(define (make-leaf v)
  (list 'leaf v))
(define (make-node w t1 t2)
  (list 'node w t1 t2))

(define (tree-size tree)
  (if (leaf? tree) 1
      (node-size tree)))

(define (link tree1 tree2)
  (list
   'node
   (+
    (tree-size tree1)
    (tree-size tree2))
   tree1 tree2))

; In an rlist, one position can be
; #f or a binary tree
; insert a tree is like
; adding one to a binary number
(define (insert-tree tree lst)
  (cond [(null? lst)
         (list tree)]
        [(not (first lst))
         (cons tree (rest lst))]
        [else
         (cons
          #f (insert-tree
              (link tree (first lst))
              (rest lst)))]))

; borrow a tree in rlist
; like subtracting one in a binary number
(define (borrow-tree lst)
  (cond [(null? lst)
         (error "Error in borrow-tree: empty list")]
        [(not (first lst)) ; here is #f
         (let* ([r (borrow-tree (rest lst))]
                [t1 (node-left (car r))]
                [t2 (node-right (car r))]
                [ts (cdr r)])
           (cons t1 (cons t2 ts)))]
        [(null? (rest lst))
         ; lst only has one element
         ; this will be (cons (first lst) '())
         ; which is lst itself
         lst]
        [else
         ; on this condition
         ; lst has multiple elements
         ; and its first element is not #f
         (cons (first lst) (cons #f (rest lst)))]))

; rlist is a list of binary-trees of #f
; cons an element onto a rlist
(define (rlist-cons val rlst)
  (insert-tree (make-leaf val) rlst))

(define (rlist-first rlist)
  (car (borrow-tree rlist)))

(define (rlist-rest rlist)
  (cdr (borrow-tree rlist)))

(define (lookup-tree t index)
  (cond [(and (leaf? t)
              (= index 0))
         (leaf-val t)]
        [(leaf? t)
         (error
          (string-append
           "Error in lookup-tree: index "
           (number->string index)
           " of a leaf."))]
        [else
         (let* ([w (node-size t)]
                [t1 (node-left t)]
                [t2 (node-right t)]
                [ni (arithmetic-shift w -1)])
           (if (< index ni)
               (lookup-tree t1 index)
               (lookup-tree t2 (- index ni))))]))

(define (rlist-lookup rlst index)
  (if (null? rlst)
      (error "Error in rlist-lookup: empty rlist.")
      (let ([h (first rlst)])
        (if h
            (let ([s (tree-size h)])
              (if (< index s)
                  (lookup-tree h index)
                  (rlist-lookup (rest rlst)
                                (- index s))))
            (rlist-lookup (rest rlst) index)))))

(define (update-tree tree index val)
  (cond [(and (leaf? tree) (= index 0))
         (make-leaf val)]
        [(leaf? tree)
         (error
          (string-append
           "Error in update-tree: index "
           (number->string index)
           " of a leaf."))]
        [else
         (let* ([w (node-size tree)]
                [t1 (node-left tree)]
                [t2 (node-right tree)]
                [ni (arithmetic-shift w -1)])
           (if (< index ni)
               (make-node
                w (update-tree t1 index val) t2)
               (make-node
                w t1 (update-tree
                      t2
                      (- index ni) val))))]))

(define (rlist-update rlst index val)
  (if (null? rlst)
      (error "Error in rlist-update: empty rlist")
      (let ([h (first rlst)])
        (if h
            (let ([s (tree-size h)])
              (if (< index s)
                  (cons (update-tree h index val)
                        (rest rlst))
                  (cons h
                        (rlist-update
                         (rest rlst)
                         (- index s) val))))
            (cons
             #f (rlist-update
                 (rest rlst) index val))))))

; =========== Tests ===================
(require rackunit)

(check-equal?
 (rlist-lookup (foldr rlist-cons '() (range 10)) 0)
 0)

(check-equal?
 (rlist-lookup (foldr rlist-cons '() (range 10)) 9)
 9)

(check-equal?
 (rlist-lookup
  (rlist-update (foldr rlist-cons '() (range 10)) 9 100)
  9) 100)

(check-equal?
 (rlist-lookup
  (rlist-update (foldr rlist-cons '() (range 10)) 9 100)
  2) 2)