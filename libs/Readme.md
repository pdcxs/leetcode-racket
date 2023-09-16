# `libs` Description

To implement efficient algorithms in a functional way, we need some interesting purely functional data structures. In `libs`, I implement the following purely functional data structures which is described in *Chris Okasaki's Purely Functional Data Structures.*:

- **Stream**: Use default stream implementation in racket, but I add some useful functions in the **stream.rkt** file. Such as `stream-reverse`, `stream-zipwith`, `stream-drop` and so on.
- **Queue**: A simple first-in-first-out (FIFO) queue. Provide `make-queue`, `queue-empty?`, `queue-first`, `queue-rest`, `enqueue` operations. I use banker's implementation as default, but also provide physitist's implementation.
- **Bums**: Abbreviation of **Bottom Up Merge Sort** data structure. This data structure only provides three operation: `make-bums`, `bums-add`, `bums-sort`. With bums, one can add an element to the bums with $O(\log N)$ and sort it with $O(N)$.
- **Dequeue**: Abbreviation of **Double Ended Queue**, which can support `dequeue-push-forward`, `dequeue-push-backward`, `dequeue-rest`, `dequeue-most`, `dequeue-first` and `dequeue-last`.
- **RList**: Abbrevaition of **Random Access List**, which support random access any position of a list with $O(\log N)$. Provide: `rlist-cons`, `rlist-lookup` and `rlist-update`. Note: an empty rlist is just an empty list, which is '(). As a result, `rlist-empty?` is just `null?`, so I do not provide it.
- **BiHeap**: Abbrevaition of **Binomial Heap**, also known as **Priority Queue**. BiHeap can provide method to find minimum element and delete minimum element. The provided method is `biheap-insert`, `biheap-merge`, `biheap-min`, `biheap-rm-min` and `biheap-size`. A typical usage of this data structure can be found in problem 630.

All of the usage example of above data structures can be found in the testing part in each source file.