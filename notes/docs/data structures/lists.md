# Lists

```lisp
;; Note dotted-pair notation unless cdr is a list/nil
(cons 1 2)
;; => (1 . 2)
(cons (1 (cons 2 nil))
;; => (1 2)
```

`first`/`rest`, `car`/`cdr` etc are supported.

Can also use `car`/`cdr` shorthands, sequences of at most 4 are supported.
E.g. `cadddr`, which equates to `(cdr (cdr (cdr (car x))))` (yes, that order).

## Destructive / Recycling functions
`list` really just outputs a series of `cons` cells. Likewise, `(append (list 1 2) (list 3 4))` can reuse the second list, creating new `cons`'s for the first list and pointing it's tail to the head of the second list.

Some functions will

### Destructive Functions
  * *for-sideeffect* functions, e.g. `vector-push`/`vector-pop`
  * "destroys" the object because its state is altered (no longer the same)

###Recycling functions
  * an optimization, really
  * will reuse `cons` cells, but (unlike `append`) alter their `car`/`cdr` as necessary.
  * only use when *sure* the original result is discarded
  * **Common-patterns**
    * `push`/`nreverse`
        * Append elements in order by repeated linking new `cons` cells to its head (`O(1)` operation), then reversing it.
    * `setf`/`delete`
        * e.g. `(setf *l* (delete nil *l*))`, a recycling variant of `remove`, reusing `cons` cells.
  * **Naming**:
    * recycling variants have 'n' prefixed their name
    * ('n' because they are *non-CONS'ing*, i.e. no new cons cells are made)
    * Exceptions exist (e.g. `nconc`, the recycling `append` variant)


#### Copy lists when needed

`sort`, `stable-sort` & `merge` are recycling functions.
Hence, use `copy-list` if the original list is to be retained.

### Common list indexing/manip functions

* `(last <lst>)`
  * return last const of list
* `(butlast <lst> [int])`
  * return list w/o last (n) cons cell(s)
* `(nbutlast lst [int])`
  * recycling variant of `butlast`
* `ldiff`
  * **TODO**
* `tailp`
  * *predicate*, ..**TODO**
* `list*`
  * 
* `make-list`
  * Builds a list, `:initial-element` can set initial values, nil otherwise
* `revappend`
  * reverses *first* argument, appends *second* argument.
* `nreconc`
  * recycling version of `revappend`
* `consp`
  * *predicate*, is the object a `cons` cell?
* `atom`
  * *predicate*, complement to `consp`
* `listp`
  * *predicate* is object a `cons` or `nil` ?
* `null`
  * *predicate* is object nil ?

### Mapping Functions
	   
  * `(map <out-type> <fn> <seq>+)`
  * `(mapcar <fn> <seq>+)`
  * `(maplist <fn> <seq+>)`
    * like `mapcar` except the `cons` cell is passed as input to the fn
  *

### Trees

  * Also a bunch of `cons` cells - difference lies in traversal.
    * *list traversal* along the `cdr`'s, the cars are list items
    * *tree traversal* along the `car`'s & `cdr`'s.

  * **Traversal example**
    * Given `((1 2) (3 4) (5 6))`
    * `copy-list` would copy only the outer cons cells (referencing the `(1 2`, `(3 4)` & `(5 6)` lists).
    * `copy-tree` would copy the entire structure of `cons` cells.

* `(tree-equal t1 t2 [:test EQL])`
  * compare two trees. Equal iff structure is identical and all elements are considered equal (using `:test` fn, `EQL` by default)
* `(subst <new-item> <old-item> tree)`
  * Tree-analog to `substitute`
  * Has `-if` & `-if-not` variants
  * Has `n` (recycling) variants
  * Substitute all elements matching old-value with new-value.
  * Accepts `:key` & `:test`

### Sets

  * `(adjoin <item> <set-lst>)`
    * return new set (list) containing `<item>`
    * Accepts `:key` & `:test`
  * `(pushnew <item> <set-lst>)`
    * Like adjoin + assignment
  * `(member <item> <set-lst>)`
    * return set iff member was found, nil otherwise
    * `-if` `-if-not` variants
    * Similar to `find` (`-if`, `if-not`)

`intersection`, `union`, `set-difference`
    