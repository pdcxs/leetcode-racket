#lang racket

; Build a path from a grid
; a path is a set of following structures:
; (row column step)
; for example
; '((0 0 0) (1 2 1))
; which means step 0 is at (0 0)
; step 1 is at (1 2)
(define (grid->path grid)
  (define n (length grid))
  ; scan the grid, i is row number,
  ; r is result.
  (define (scan grid i r)
    (if (null? grid) r
        ; create a loop
        ; iterate over each column
        ; of the i-th row
        (let loop ([row (car grid)]
                   [j 0]
                   [r r])
          (if (null? row)
              (scan (cdr grid)
                    (add1 i) r)
              (loop
               (cdr row)
               (add1 j)
               (cons
                (list i j (car row)) r))))))
  (let ([path (scan grid 0 '())])
    ; we sort the path according to the
    ; step number
    (sort path < #:key third)))

; Decide whether two adjacent
; steps are valid.
(define (can-move? step1 step2)
  (let ([r1 (first step1)]
        [c1 (second step1)]
        [r2 (first step2)]
        [c2 (second step2)])
    (or
     (and (= (abs (- r1 r2)) 1)
          (= (abs (- c1 c2)) 2))
     (and (= (abs (- r1 r2)) 2)
          (= (abs (- c1 c2)) 1)))))

; Main function
(define (check-valid-grid grid)
  (define r
    (let ([path (grid->path grid)])
      ; We need to check whether the first
      ; step is at the left top corner.
      (if (not (= (first (first grid)) 0))
          #false
          ; use foldl to collect
          ; the result
          (foldl
           (λ (step acc)
             ; If previous step is not false
             ; and from previous step
             ; can move to step
             ; then return step
             ; else return false
             (and acc
                  (can-move? acc step)
                  step))
           (first path) (rest path)))))
  (if r #true #false))

; ============ Tests ================
(require rackunit)

(check-equal?
 (check-valid-grid
  '((0 11 16 5 20)
    (17 4 19 10 15)
    (12 1 8 21 6)
    (3 18 23 14 9)
    (24 13 2 7 22)))
 #true)

(check-equal?
 (check-valid-grid
  '((0 3 6)
    (5 8 1)
    (2 7 4)))
 #false)