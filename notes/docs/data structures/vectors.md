# Vectors
Vectors are an integer-indexed, array-like type of [sequence](sequences.md).


  * integer-indexed
  * (typically) array-like, managing some contiguous memory
  * Variants:
    * fixed size: `(make-array <size> :initial-element nil)`
    * resizable, fixed upper-bound: `(make-array <size> :fill-pointer 0)`
    * resizable: `(make-array <init-size> :fill-pointer 0 :adjustable t)`
  * Can hold mixed types
  * Restrict to holding one type: `(make-array ... :element-type 'character)`
    * (this, incidentally, creates a resizable string)

**Note**

  * use `:initial-element <value>` to initialize vector items with some value
    * not doing so will cause errors if an item is accessed before it's defined.
  * `:adjustable t` is what makes a vector act as a Java-style `ArrayList` allocating a new array when needed
  * Reader syntax for a vector: `#(1 2 3)` is equal to `(vector 1 2 3)`
    * result is a fixed-size vector.