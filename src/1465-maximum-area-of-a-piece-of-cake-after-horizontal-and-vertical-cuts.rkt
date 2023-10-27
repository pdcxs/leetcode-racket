#lang racket

; ============== Method 1 ================
; I think this is very fast
; but this method consumes too much memory.
; The complexity of this method is O(N).

; if cut at location i, we set the i-th bit
; of record number to 1. Iterate all cuts
; can get the record.
; then only count the maximum number of
; successive zeros of the record bits
; can obtain the maximum cut distance.
; Without sort, we only need O(N) to solve
; the problem.

(define (build-record max cuts)
  (define (scan cuts record)
    (if (null? cuts)
        record
        (scan (rest cuts)
              (bitwise-ior
               record
               (arithmetic-shift
                1 (first cuts))))))
  (define init-record
    (bitwise-ior
     (arithmetic-shift 1 max) 1))
  (scan cuts init-record))

(define (count-max-successive-zero record)
  (define (scan record current-num max-num)
    (if (zero? record) (max current-num max-num)
        (let ([bit (bitwise-and record 1)]
              [rbits (arithmetic-shift record -1)])
          (if (= bit 0)
              (scan rbits (add1 current-num) max-num)
              (scan rbits 0 (max current-num max-num))))))
  (scan record 0 0))

(define (max-area-method1 h w horizontalCuts verticalCuts)
  (define h-record (build-record h horizontalCuts))
  (define v-record (build-record w verticalCuts))
  (let ([hm (count-max-successive-zero h-record)]
        [vm (count-max-successive-zero v-record)])
    (remainder (* (add1 hm) (add1 vm)) 1000000007)))

; ============ Method 2 ===============
; Just use sort.
; This is simple but it just works.

(define (max-cut max-size cuts)
  (define sorted-cuts (sort cuts <))
  (define (scan cuts m)
    (match cuts
      [(cons x (cons y rs))
       (scan (rest cuts) (max m (- y x)))]
      [(cons x '())
       (max m (- max-size x))]
      [else
       (error "Null Cuts!")]))
  (scan sorted-cuts (first sorted-cuts)))

(define (max-area h w horizontalCuts verticalCuts)
  (remainder
   (* (max-cut h horizontalCuts)
      (max-cut w verticalCuts)) 1000000007))

; ==================== Test ======================
(require rackunit)

(check-equal?
 (max-area 5 4 '(1 2 4) '(1 3))
 4)
(check-equal?
 (max-area 5 4 '(3 1) '(1))
 6)
(check-equal?
 (max-area 5 4 '(3) '(3))
 9)
