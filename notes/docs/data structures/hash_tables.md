# Hash Tables

### Make a hash table

* `(make-hash-table)`
  * Create new hash table
  * By default, key-equality test is `eql` - won't work for string comparisons
  * Override equality test function with with `:test`
  * possible `:test` values: `eq`/`eql`/`equal`/`equalp`

### Extracting/Setting Values

* `(gethash <key> <hash-table>)`
  * Retrieve value indexed by key (or nil)
  * Returns multiple values `(key, exists)` to disambiguiate deliberately `nil` values.
  * Check for value existence:
    * `(multiple-value-bind (val exists) (gethash ...) ...)`

### Iterating

#### Using maphash

* `(maphash <k-v-fn> <hash>)`
  * apply `<k-v-fn>` to each entry (key & value) of the hash

#### Using LOOP macro

```lisp
(loop for k being the hash-keys in mytbl using (hash-value v)
  do (format t "~a => ~a~%" k v))
```

#### On updating the hash while iterating

* Generally: behaviour is unspecified (i.e. *don't*), except when:
  * Using `setf` + `gethash` to change the value of the current entry
  * Using `(remhash <key> <table>)` to remove the *current* entry