#lang racket

(define shift-size 2147483648) ; (expt 2 31)

; We use two bits to represent each bit
(define (num->twobits n)
  (define unsigned-n (+ n shift-size))
  (define (scan num k)
    (if (zero? k) '()
        (cons (bitwise-and num 1)
              (scan (arithmetic-shift num -1)
                    (sub1 k)))))
  (scan unsigned-n 32))

(define (twobits->num lst)
  (define (scan lst base n)
    (if (null? lst) n
        (let ([f (first lst)]
              [r (rest lst)])
          (scan r
                (* 2 base)
                (+ n
                   (* (if (not (= 1 f)) 0 1)
                      base))))))
  (- (scan lst 1 0) shift-size))

(define (twobits-add lst1 lst2)
  (if (null? lst1) '()
      (cons
       (remainder (+ (first lst1)
                     (first lst2)) 3)
       (twobits-add (rest lst1)
                    (rest lst2)))))

(define (single-number nums)
  (twobits->num
   (foldl (λ (x acc)
            (twobits-add (num->twobits x) acc))
          (make-list 32 0) nums)))

; ============== Method 2 ============
(define (single-number-circuit nums)
  (define (scan nums a b)
    (if (null? nums) b
        (let* ([n (first nums)]
               [nb (bitwise-and (bitwise-not a)
                                (bitwise-xor b n))]
               [na (bitwise-and (bitwise-not nb)
                                (bitwise-xor a n))])
          (scan (rest nums) na nb))))
  (scan nums 0 0))

; ============ Test ============
(require rackunit)

(check-equal?
 (single-number '(2 2 3 2)) 3)

(check-equal?
 (single-number '(0 1 0 1 0 1 99)) 99)

(check-equal?
 (single-number-circuit '(2 2 3 2)) 3)

(check-equal?
 (single-number-circuit '(0 1 0 1 0 1 99)) 99)