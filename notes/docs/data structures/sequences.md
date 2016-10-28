# Sequences
Not to be confused with [FSet sequences](https://common-lisp.net/project/fset/Site/FSet-Tutorial.html),
sequences are the base type shared by lists and vectors both.

## Basics (length/get an element)
  * `(length <seq>)` - length of sequence (for lists, this means full traversal)
  * `(elt <seq> <ndx>` - get nth element of sequence. setf-able place.

## Five central sequence functions
  * `(count <item> <seq>)` -- number of times item appears in sequence
  * `(find <item> <seq>)` -- item or nil
  * `(position <item> <seq>)` -- index into sequence or nil
  * `(remove <item> <seq>)` -- sequence with instances of <item> removed (shallow copy)
  * `(substitute <new-item> <item> <seq>)` -- sequence with instances of <item> replaced by <new-item>

These five functions accept the following keyword arguments

  * `:test` - two-arg fn to compare elements (or value extracted by `:key` fn)
  * `:key` - one-arg fn to extract key value from a sequence element
  * `:start` - start-index (inclusive) of subsequence to examine
  * `:end` - end-index (exclusive) of subsequence to examine
  * `from-end` - if true, traverse in reverse order (end->start)
  * `:count` - (`remove`/`substitute` only) - number of elements to remove/substitute. `nil` indicates all

### Higher-order variants (-if/-if-not)
Each of the five functions have two higher-order variants. Swap out
`<item>` for a single-arg function and add the suffixes `-if` or `if-not`.

Example:
```lisp
(defparameter *x* (vector 1 2 3 4 5 6))
(remove-if #'evenp *x*)
;;=> #(1 3 5)
(remove-if-not #'evenp *x*)
;;=> #(2 4 6)
```

**NOTE:** The higher-order variants accept the same keyword arguments *except* `:test`.

## Creating new sequences

  * `(copy-seq <seq>)` - return copy of sequence
    * shallow copy
    * same type as input sequence
  * `(reverse <seq>)` - sequence with elements in reverse order
    * shallow copy
  * `(concatenate <out-type> <seq>+)`  - concatenate sequences into one
    * must specify output type (e.g. `'string`, `'list`)

Observations
  * `(concatenate)` - works like Clojure's `(into)`, except multiple seqs can be joined

### Subsequences
subseq, fill, search, mismatch

  * `(subseq seq start [end])`
    * extract subsequence
    * is `setf`-able, though the shortest of the two (subsequence range & new sequence) determines the number of elements overwritten.
  * `(fill seq <val>)`
    * set each element in the sequence to `<val>`
    * Use `:start` & `:end` to target a subsequence
  * `(search <subseq> <seq>)`
    * find index of first occurrence of `<subseq>` in `<seq` (or nil)
    * Like `position`, except a subsequence is supplied instead of an item

### Sort & Merge

  * `(sort <seq> <fn>)`
    * `(stable-sort)` variant guaranteed not to reorder equal elements
    * Is a [destructive function](../terminology.md#destructive-functions)
    * Takes a `:key` fn, a one-arg fn called on each element, its result passed to the comparison fn
      * (works just like `:key` for the five functions discussed earlier)
  * `(merge <out-type> <seq1> <seq2> <predicate-fn>)`
    * predicate fn: `(seq1-item seq2-item -> t/nil)`
      * Used to sort the resulting sequence. True means `seq1-item` is next in the output sequence.
    * <out-type>: some symbol denoting the intended output type

Observations:
  * `(sort ... :key <fn>)` -- works like Clojure's `(sort-by)`

## Predicate FN's

* General form: `(<...> <predicate-fn> <sequence>+)`
* predicate function takes as many arguments as sequences supplied
* stops once any sequence has been exhausted
* each test examines elements in the same index
  * e.g. test 1 applies the predicate with the first element from each sequence etc..

The predicate functions are:

  * `every`
    * true iff the predicate function held for all tests
  * `some`
    * true iff the predicate function held for one or more tests
  * `notany`
    * true iff the predicate failed for all tests
  * `notevery`
    * true iff the predicate failed for one or more tests

## Mapping FN's (map/reduce)
covers `map`, `map-into` and `reduce`.

  * `(map <seq-type> <map-fn> <sequence>+)`
    * Takes n sequences and an n-argument mapping function
    * terminates when the shortest sequence is exhausted
    * applies elements in pairs (e.g. first call with elements at index 0 from each sequence)
    * Maps into a data structure determined by `<seq-type>`, e.g. `'vector`/`'list`
  * `(map-into <seq> <map-fn> <sequence>+)`
    * Exactly like `map` except that results are mapped into existing sequence `<seq>`
    * won't resize adjustable vectors - so may terminate before even the shortest sequence is exhausted if the input sequence is shorter still.
  * `(reduce <fn> <seq>)`
    * Regular reduce, distill sequence of values down into one aggregate value
    * `:initial-value` can supply a starting value. Equivalent to prepending it to the head of the sequence
    * Supports `:key`, `:start`, `:end`, `:from-end`