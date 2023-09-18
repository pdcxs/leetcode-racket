#lang racket

; main function
(define (convert s row-num)
  (define (collect-results r)
    (list->string
     (apply append
            (map reverse r))))
  (define n (- (* row-num 2) 2))
  (define (scan i lst results)
    (let ([next (+ i 1)]
          [rem (remainder i n)])
      (cond [(null? lst) results]
            [(<= rem (/ n 2))
             (scan
              next
              (rest lst)
              (add-to-nth
               rem results (first lst)))]
            [else
             (scan
              next
              (rest lst)
              (add-to-nth
               (- n rem) results
               (first lst)))])))
  (if (= row-num 1)
      s
      (let ([r
             (scan 0 (string->list s)
                   (make-empty-list row-num))])
        (collect-results r))))

(define (make-empty-list n)
  (build-list n (λ (i) '())))

(define (add-to-nth n lst e)
  (if (= n 0)
      (cons (cons e (car lst))
            (rest lst))
      (cons (first lst)
            (add-to-nth
             (- n 1) (rest lst) e))))

; ============= test ==============
(require rackunit)
(check-equal?
 (convert "PAYPALISHIRING" 3)
 "PAHNAPLSIIGYIR")

(check-equal?
 (convert "PAYPALISHIRING" 4)
 "PINALSIGYAHRPI")

(check-equal?
 (convert "A" 1)
 "A")