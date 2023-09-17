#lang racket

(require "../libs/rlist.rkt")

; Using random access list in this problem
; is a little bit slower than the normal method,
; which only use cons list.
; However, if the size of input becomes larger,
; I believe this method should be faster,
; Because the complexity of list-ref is O(N).
; The complexity of rlist-lookup is O(log N).

(define (find-median-sorted-arrays nums1 nums2)
  (define (merge nums1 nums2)
    (cond [(null? nums1)
           (foldr rlist-cons '() nums2)]
          [(null? nums2)
           (foldr rlist-cons '() nums1)]
          [(< (first nums1) (first nums2))
           (rlist-cons (first nums1)
                       (merge (rest nums1) nums2))]
          [else
           (rlist-cons (first nums2)
                       (merge nums1 (rest nums2)))]))
  (let* ([r (merge nums1 nums2)]
         [s (rlist-length r)]
         [m (arithmetic-shift s -1)])
    (if (even? s) 
        (/ (+ (rlist-lookup r m)
              (rlist-lookup r (sub1 m))) 2.0)
        (rlist-lookup r m))))

; Normal method, which not requires rlist.
(define (normal-method nums1 nums2)
  (define (merge nums1 nums2)
    (cond
      [(null? nums1) nums2]
      [(null? nums2) nums1]
      [else
       (let ((n1 (car nums1))
             (n2 (car nums2)))
         (if (< n1 n2)
             (cons n1 (merge (cdr nums1) nums2))
             (cons n2 (merge nums1 (cdr nums2)))))]))

  (let* ([merged-nums (merge nums1 nums2)]
         [len (length merged-nums)])
    (if (odd? len)
        (list-ref merged-nums (- (/ (+ len 1) 2) 1))
        (/ (+ (list-ref merged-nums
                        (/ len 2))
              (list-ref merged-nums
                        (- (/ len 2) 1))) 2.0))))

; =============== Test ====================
(require rackunit)

(check-equal?
 (find-median-sorted-arrays '(1 3) '(2)) 2)

(check-equal?
 (find-median-sorted-arrays '(1 2) '(3 4)) 2.5)

(check-equal?
 (normal-method '(1 3) '(2)) 2)

(check-equal?
 (normal-method '(1 2) '(3 4)) 2.5)