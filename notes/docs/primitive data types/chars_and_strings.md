# Characters & Strings

## Characters

  * Distinct from numbers (unlike e.g. C) - e.g. not tied to encoding
  * Reader syntax `#\x` would yield the character `x`

Certain characters, e.g. space, return, backspace, tab, page, linefeed can also
be written out, e.g. `#\space`.

### Comparison

* `char=`
* `char/=`
* `char<`
* `char>`
* `char<=`
* `char>=`

... basically, prefix the equivalent numerical comparison function with `char`.

Case-insensitive variants also exist, write out the operator e.g. the
case-insentive variant of `char/=` is `char-not-equal`.

Only deviation from that system is `char>=`/`char<=` which are written
as `char-not-{greater,less}p`.

**Like numerical comparison functions, these functions function in the same
way as their numerical counterparts when supplied multiple char arguments.**

## Strings

Strings are a type of sequences (base type underlying lists) hence a sequence
functions will also work on strings.

This section covers stirng-specific functions, though.

### Comparison
(Identical to `char` section)

* `string=`
* `string/=`
* `string<`
* `string>`
* `string<=`
* `string>=`

... basically, prefix the equivalent numerical comparison function with `string`.

Case-insensitive variants also exist, write out the operator e.g. the
case-insentive variant of `string/=` is `string-not-equal`.

Only deviation from that system is `string>=`/`string<=` which are written
as `string-not-{greater,less}p`.

**Unlike char/numerical functions, these compare two strings only**. This is
because they accept keyword arguments `:start1`, `end1`, `start2`, `end2` which
enables substring comparisons if desired.

```
(string= "xoxocakexo" "cake" :start1 4 :end1 8)
;; => T
```

That is, substring arguments define a slice like so: `[start;end[`.

# TODO
string concat, trim