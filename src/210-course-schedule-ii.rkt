#lang racket

(require "../libs/graph.rkt")

(define (find-order numCourses prerequisites)
  (define verts (in-range numCourses))

  (define (scan scc result)
    (stream->list scc))
  
  (let* ([graph (build-graph prerequisites)]
         [st (top-sort graph verts)]
         [scc (dfs graph st)])
    (scan scc '())))
  

(find-order 4 '((1 0) (2 0) (3 1) (3 2)))
(find-order 2 '((0 1) (1 0)))