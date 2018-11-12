# linear-containers

`linear-containers` allows users to use data structures
that can be modified in-place.

The base of `linear-containers` is the module `Linear.Reference`
which implements a malloc/free interface that cannot leak memory.
Using `Linear.Reference` as a base, we can extend
many built-in data structures so that they can be modified in-place.
The new data structures will have better performance than the
built-in ones when they reach large sizes (example > 1 GB).
The increase in performance results from a lot less 
Garbage Collection (GC) pauses.

Current data structures extended:
 - Linked List
