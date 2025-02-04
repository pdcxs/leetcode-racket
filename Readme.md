# Purely Functional Solutions of LeetCode Problems

I wonder if there exist some methods to solve the algorithm problems of LeetCode in a purely functional way. As a result, I created this project. I use a lot of purely functional data structures described in *Chris Okasaki's Purely Functional Data Structures.* If someone wants to see the detailed implementation, please refer to the `libs` directory.

To see how to use the libs, please check the test parts in each file.

Each solution file contains some tests, which is not a part of the solution itself.

Many Problems need to memorize the computation results. Sometimes I use mutable hash tables to memorize the results. In the next stage, I will try to use a purely functional way to implement memoization. Remembering the results in a purely functional way is possible, which can be found in [memoize](https://hackage.haskell.org/package/memoize) package in Haskell. Following is an example:

```racket
(define (memoize f)
  (define results (stream-map f (in-naturals)))
  (lambda (n) (stream-ref results n)))

(define fib
  (memoize
   (λ (n)
     (cond [(= n 0) 1]
           [(= n 1) 1]
           [else (+ (fib (- n 1))
                    (fib (- n 2)))]))))
```

I'm a newbie to the `racket` programming language. So any suggestions and PR are welcomed.
