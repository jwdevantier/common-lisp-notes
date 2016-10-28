#Numbers

#### Textual vs object representation
A given number may have many textual represenations. E.g. `10`, `20/2` `#xA` are
all textual representations of the same number. (Yes, `20/2` is really a number,
a fraction, to be precise).

Equivalent textual representations all get translated to the same object
representation by the *lisp reader*.

*This fact, that many textual representations lead to the same object
representation, is why a number may be outputted differently - this is because
each object in lisp has a canonical representation used by the *lisp printer*.

##### Different base

* **binary** `#b110` is 6
* **hex** `#xa` is 10
* **octal** `#o777` is 511
* **???** `#r` ???

#### Math functions

##### Multiple arguments to math functions
`+`,`-`,`/` etc take an arbitrary number of arguments. If used, application
works as a reduce, creating a result from the first two args which is then
applied with the third argument etc...

##### Truncating numbers
`/` doesn't truncat, but CL provides functions for doing so:

  * `floor` -- rounds down, e.g. largest int smaller- or equal to value
  * `ceiling` -- rounds up -- e.g. smallest int larger- or equal to value
  * `truncate` -- truncates toward zero, e.g. `floor` for positive values, `ceiling` for negative ones.

##### Division

  * `mod` -- return modulus of division
  * `rem` -- return remainder of division

**NOTE**: these are equivalent for positive numbers, only.

#### Comparison

`=` is the numeric comparison function. Unlike `eql` (and like `equalp`), `=`
will yield true when comparing two numerically identical values of different
types.

`/=` -- true iff. all supplied values are distinct.

`>`, `>=` ,`<`, `<=` work as expected with two arguments. **With multiple
arguments**, each value is compared with the value to its immediate right.

##### Comparison to zero

  * `zerop`
  * `plusp`
  * `minusp`

##### Min & Max

  * `min` -- yield smallest number among those supplied
  * `max` -- yield largest number among those supplied
