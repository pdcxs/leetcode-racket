#lang racket

(define (get-number-list s)
  (take-while
   digit?
   (string->list s)))

(define (get-sign-number-list s)
  (if (string=? s "")
      '()
      (let ([c (string-ref s 0)])
        (if (or (char=? c #\+)
                (char=? c #\-))
            (cons c (get-number-list (substring s 1)))
            (get-number-list s)))))

(define (get-number-string s)
  (list->string (get-sign-number-list s)))

(define (digit? c)
  (char<=? #\0 c #\9))

(define (take-while pred ls)
  (cond [(null? ls) ls]
        [(pred (car ls))
         (cons (car ls)
               (take-while pred (cdr ls)))]
        [else '()]))

(define (limit x)
  (cond [(< x -2147483648) -2147483648]
        [(> x 2147483647) 2147483647]
        [else x]))

(define (my-atoi s)
  (let ([m (get-number-string (string-trim s))])
    (if (or (string=? m "")
            (string=? m "-")
            (string=? m "+"))
        0
        (limit (string->number m)))))

; ================ Test ==================
(require rackunit)
(check-eq?
 (my-atoi "42") 42)
(check-eq?
 (my-atoi "   -42") -42)
(check-eq?
 (my-atoi "4193 with words") 4193)