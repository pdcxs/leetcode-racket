#lang racket

(define (queens-attackthe-king queens king)
  (filter (λ (q) (can-attack q king queens)) queens))

; check whether a queen can attack a king
(define (can-attack queen king queens)
  (let ([p (attack-path queen king)])
    (and
     (attack-pos? queen king)
     (null? (intersect p queens)))))

; find wheter two cells is on the attack position
(define (attack-pos? c1 c2)
  (or
   (= (first c1) (first c2))
   (= (second c1) (second c2))
   (= (abs (- (first c1) (first c2)))
      (abs (- (second c1) (second c2))))))

; find the cells on the attack path
; from queue to king
(define (attack-path queen king)
  (cond
    ; Same row?
    [(= (first queen) (first king))
     (row-cell-between queen king)]
    ; Same column?
    [(= (second queen) (second king))
     (col-cell-between queen king)]
    ; Diagonal?
    [(= (abs (- (first queen) (first king)))
        (abs (- (second queen) (second king))))
     (diag-cell-between queen king)]
    ; Cannot attack
    [else '()]))

; Take the cell between two cells in same row
(define (row-cell-between c1 c2)
  (let ([step (if (> (- (second c1)
                        (second c2)) 0)
                  -1 1)]
        [r (first c1)])
    (for/list ([i (in-range
                   (+ (second c1) step)
                   (second c2) step)])
      (list r i))))

; Take the cell between two cells in same column
(define (col-cell-between c1 c2)
  (let ([step (if (> (- (first c1)
                        (first c2)) 0)
                  -1 1)]
        [c (second c1)])
    (for/list ([i (in-range
                   (+ (first c1) step)
                   (first c2) step)])
      (list i c))))

; Take the cell between two cells in diag
(define (diag-cell-between c1 c2)
  (let ([r-step (if (> (- (first c1)
                        (first c2)) 0)
                  -1 1)]
        [c-step (if (> (- (second c1)
                        (second c2)) 0)
                  -1 1)]
        [c (second c1)])
    (for/list ([i (in-range
                   (+ (first c1) r-step)
                   (first c2) r-step)]
               [j (in-range
                   (+ (second c1) c-step)
                   (second c2) c-step)])
      (list i j))))

; intersection of two lists
(define (intersect lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (if (member (car lst1) lst2)
          (cons (car lst1)
                (intersect (cdr lst1) lst2))
          (intersect (cdr lst1) lst2))))

; ================= Tests ===============
(require rackunit)
(define queens
  '(
    ((0 1) 
     (1 0)
     (4 3)
     (0 4)
     (3 3)
     (2 4))
    ((0 0)
     (1 1)
     (2 2)
     (3 4)
     (3 5)
     (4 4)
     (4 5))
    ((5 6)
     (7 7)
     (2 1)
     (0 7)
     (1 6)
     (5 1)
     (3 7)
     (0 3)
     (4 0)
     (1 2)
     (6 3)
     (5 0)
     (0 4)
     (2 2)
     (1 1)
     (6 4)
     (5 4)
     (0 0)
     (2 6)
     (4 5)
     (5 2)
     (1 4)
     (7 5)
     (2 3)
     (0 5)
     (4 2)
     (1 0)
     (2 7)
     (0 1)
     (4 6)
     (6 1)
     (0 6)
     (4 3)
     (1 7))))
(define kings '((0 0) (3 3) (3 4)))
(define outputs
  '(
    ((0 1) (1 0) (3 3))
    ((2 2) (3 4) (4 4))
    ((2 3) (1 4) (1 6) (3 7) (4 3) (5 4) (4 5))))

(define (list-comp lst1 lst2)
  (if (= (first lst1) (first lst2))
      (< (second lst1) (second lst2))
      (< (first lst1) (first lst2))))

(for ([q queens]
      [k kings]
      [o outputs])
  (check-equal?
   (sort (queens-attackthe-king q k) list-comp)
   (sort o list-comp)))