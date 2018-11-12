# linear-containers

`linear-containers` allows users to use data structures
that can be modified in-line.

The base of `linear-containers` is the module `Linear.Reference`
which has functions that use `malloc` and `free` without causing
memory leaks. Using `Linear.Reference` as a base, we can extend
many built-in data structures so that they can be modified in-line.
The new data structures will have better performance than the
built-in when they reach large sizes (example > 1 GB)

Current data structures extended:
 - Linked List
