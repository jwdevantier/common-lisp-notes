# Functions

## Syntax Overview

```
(defun foo (param1 &optional opt-param1 opt-param2 &rest rest &key fooparam barparam)
  "docstring goes here"
  (return-from foo (+ 2 3)))
```

## Parameters

### Required
```
(defun sum (a b)
  (+ a b))
```

### Optional

```
(defun foo (a &optional (b 10) c)
  (list a b c))
```

 * `(foo 2)` => `(2 10 nil)`
 * `(foo 2 7 98)` => `(2 7 98)`

That is, optional parameters are bound to supplied parameters, in order. If no argument
is supplied, the optional parameter is bound to `nil` unless a default value is supplied as is the case for `b` above.

Like `&key` params, the default value list can take 3 elements: `(<sym> <defval> <was-supplied-p>)`. The third parameter can be used to see if a value was provided, which can disambiguiate situations where a `nil` might be a legal input value.

**NOTE** The default-value expression can actually refer to earlier parameters, e.g.
```
(defun area-rect (w &optional (h w h-p))
  (when h-p (format t "height was provided"))
  (* w h))
```

This fn will default to calculating the area of a square, since `h` will take the same value as `w` unless a value is supplied.

### Rest (varargs)
Rest functions as varargs in other languages, the symbol following `&rest` becomes a list
of any parameters which are supplied *in addition* to those required by the function.
```
(defun msum (a &rest vals)
  (apply #'+ (cons a vals)))
```

  * `(msum 1)` => `1`
  * `(msum 1 2 3)` => `6`

### Keyword

  * default value, unless specified, is `nil`
  * (unlike python) must specify the key when calling

```
(defun foo (&key (a 10) ((:second b) 17 b?) c)
    (list a b b? c))
```

  * `(foo)` => `(10 17 nil nil)`
  * `(foo :second 2 :c 9 :a -1)` => `(-1 2 T 9)`

The only destructuring form different from `&optional` is the ability to name
the parameter differently in the API. Externally, `:second` is used while in
the function, `b` is used.

## Ordering of parameter types
You can combine all four parameter types in the same function. Though the ordering
is strict:

1. required parameters
2. optional parameters (if any)
3. rest parameter
4. keyword parameters

**NOTE** if *rest* & *keyword* parameters are mixed, your rest parameter will be a plist
of all the keyword parameters.

## Return from a function

  * `(return <retval>)`
  * `(return-from <label> <retval>)`

**NOTE**: actually, `(return)` & `(return-from <label>)` doesn't return from
functions, but blocks. Like `break [<label>]` in C.

```
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
```

## Invoking/getting functions from symbols (funcall/apply/function)

  * `(function x)`/`#'x` -- gets the fn from a symbol
  * `(apply <fn> <list-of-args>)`
    * use to call a functionby its symbol name when you don't know the exact number and order of the arguments (but you can get a list of them)
    * **Actually accepts loose arguments**, though **last argument must be a list**
  * `(funcall <fn> ...<args>...)` (`(<fn> ...<args>...)`)
    * use to call a function by its symbol name (e.g. as function arg)

For purposes of disambiguiation, `#'list` refers to the *function* 'list'
whereas `list` refers to a *variable*. This is due to CL's lisp-2 heritage
where function & variable namespaces are separate.

## Generic Functions

Part of the CLOS (Common Lisp Object System), generic functions exemplify a
different approach to Object-oriented programming than seen in C++ and
derivatives.


### Definining a generic function

Think of `defgeneric` as akin to defining an interface for a function - no
concrete implementation is provided.

```lisp
(defgeneric draw (shape)
  (:documentation "Draw the given shape onto the screen."))
```

**NOTE:** while, technically, `defgeneric` can be omitted (it will be generated
based on the `defmethod`'s supplied), it's bad form.

The `:documentation` is the most frequently defined option, but the [CLHS entry](http://clhs.lisp.se/Body/m_defgen.htm#defgeneric) defines others. Most are advanced, allowing to alter the method-combination, assign the generic a different class (`STANDARD-GENERIC-FUNCTION`) etc.

### Implementing a generic function

```lisp
;; Note this follows *exactly* a normal method
(defmethod draw (shape)
  ...)
```

#### Selecting the right method
Because multiple `defmethod`'s may exist, CL needs to decide which to invoke.
To that end, CL determines the *most* specific method based on comparing the
available implementations to the arguments received.

Methods are specialized by applying *specializers* to its arguments.

##### Argument specializers
Arguments can be *specialised* by replacing the variable by a two-item list.
Two types of specializers exist, **type-** and **eql-specializers**.

**NOTE:** methods that specialize more than one argument are called **multimethods**.

###### Examples
  * Type specializer: `(shape triangle)`
  * EQL specializer: `(shape (eql *my-special-shape-obj*))`
    * **NOTE** `*my-special-shape-obj*` is resolved to a value when parsing the `defmethod` - hence changes to the variable won't be reflected for method invocation.

###### Specificity of specializers
From most to least specific

  * matching eql-specializer
    * => if multiple implementations share the same eql-specializer on the same param, the other parameters will decide which implementation is selected.
  * type specializer matching the object's exact type
  * type specializer matching a super type of the object
  * un-specialized parameter
    * (implicitly specialized on `T`, Lisp's root type)

#### Method Combination / The Effective Method
CL won't select a single implementation method, rather, the *effective method*, that is the method which is built from one or more implementations (more in a minute) is built in a 3-step process:

  1. the generic function builds a list of applicable methods based on actual arguments received.
  2. the applicable methods list is sorted by *specificity* of their argument specializers.
  3. methods are taken, in-order, and their code combined, to produce the effective method.

The methods discussed so far are *primary methods*, but the effective method also includes *auxiliary methods*, defined as regular methods but "tagged" `:before`, `:after`, or `:around`.

As implied, multiple (primary) methods can be combined in the effective method.
This is done by having one method call the next-most specific method via
`(call-next-method)`. If no arguments are provided, `(call-next-method)` passed
the input arguments from the current method along.

  * **primary method**
    * implementations defined with `defmethod` as seen above
  * **auxiliary method**
    * `defmethod` definitions with a keyword: `:before`, `:after`, `:around`
  * **effective method**
    * the result from ordering applicable (primary) methods and considering the auxiliary methods.

#### Auxiliary Methods
As explained above, primary methods may be complemented by `:before`, `:around`
and `:after` methods.

  * `:before`
    * run all before-methods in most-specific-first order
  * `:after`
    * run all after-methods in most-specific-last order (reverse of `:before`)
  * `:around`
    * functions like a wrapper function
    * Somewhere in the `:around` method, call `(call-next-method)`

```lisp
;; note the keyword following the method identifier.
(defmethod foo :before ((x bar) y)
  ...)
```

#### Other Method Combinations
The standard method combination, explained above, combines methods from most- to
least specific, optionally letting them chain with `(call-next-method)`.

However, CL implements *9* additional method combinations, which combines *all*
applicable methods, wrapping them in some function/macro call which also gives
the method combination its name.

These are: `+`, `and`, `or`, `list`, `append`, `nconc`, `min`, `max` and `progn`.
E.g. `+` will sum all outputs from the primary methods while `and` will execute
each primary method unless short-circuiting on a nil value. And so on.

To use one of these, set `:method-combination <...>` in `defgeneric`. Also, each
method needs to reflect the method-combination used, hence:

```lisp
;; note how the method-combinator '+' follows the identifier
(defmethod expenses + ((f food)) 3500)
```

The ordering of methods is still from most- to least specific, though
`:most-specific-last` can be appended to the `:method-combination` option in
`defgeneric` to reverse the order.

Finally, note that `:before` and `:after` auxiliary methods aren't supported for
any of these method combinations. `:around` methods can still be used though
`:most-specific-last` will not change their invocation order.

