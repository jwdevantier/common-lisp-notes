# Variables

  * dynamically typed (errors are detected dynamically, at run-time)
  * strongly typed

##### Variable >< Binding
Each time a fn is invoked, CL creates new *bindings* to hold the arguments
passed by the caller.
That is, a variable may have multiple bindings over the course of a program
even at the same time (e.g. recursive functions).

##### Binding Form
A lisp form which introduces new bindings, `let` is a binding form, so are
functions.

## Lexical variables
Lexical scoping refers to a type of scoping where you can (generally, see
closures) infer where a variable can be used from reading the program text.

(Hence *lexical*)

##### Scope
The area of the program where the variable name can be used to refer to the
variable's binding.

##### Shadowing
The result of nesting two binding forms where one (or more) variable identifiers
are shared between the binding forms.

```
(let ((x 10))
  (let ((x 12))
    (+ x 2)))
;; => 14
```
In this example, the innermost x binding shadow the outermost binding.

##### Closure
```
(let ((count 0))
  #'(lambda ()
      (setf count (+1 count))))
```

Holding onto, and eventually invoking the lambda will work because it *closes*
over the binding created by `let`.

We sometimes call these constructs (functions) *closures*.

**NOTE** what's captured by a closure is the *binding*, not the value - changes
will be visible from other closures referencing the same binding.

## Dynamic Variables

Two ways to define dynamic variables; `defvar` and `defparameter`.

`defvar` won't override an existing binding, thereby functioning like
Clojure's `defonce`. Use it for fields where the value should be
retained even when the file is reloaded.

```
(defvar <identifier> <val> [<docstring>])
```

`defparameter` always assigns the initial value given.
```
(defparameter <identifier> <val> [<docstring>])
```
##### Extent
Dynamic variables can be overridden for the *extent* of some binding form.
Unlike *scope*, this overridden value is visible to any functions called
which refers to the dynamic variable.
E.g. consider it a way to override *stdout*, allowing redirection of stdout
without altering a bunch of functions to take an output steam parameter.

This works like Racket's dynamic bindings, which can be [parameterized](https://docs.racket-lang.org/guide/parameterize.html).

Conceptually, each new binding for a dynamic variable is pushed onto a stack
of bindings for that variable; escaping a binding form pops a binding off
the stack and references always consult the topmost binding.
E.g. it functions much the same as nested `let`-bindings for lexical variables.

**NOTE** this behaviour is *always* automatically triggered when a `let`-binding
attempts to use a variable name that's been declared *special* - and
`defvar` and `defparameter` both do this.

Hence, by convention, `defvar`/`defparameter` variablenames are surrounded by
asterisks (*), e.g. `*foo*` rather than `foo`.
In this way, code won't mistakenly tamper with dynamic bindings.

Locally dynamic variables exist - see `declare`, `special` and `locally`.

**Only use dynamic variables if**: you need to modify behavior of downstream
code (and threading the parameter isn't desirable) **and/or** downstream code
should be able to affect the value of a binding higher up the callstack.

## Constants

```
(defconstant <identifier> <val> [<docstring>])
```

By convention, constant names start and end with `+`, e.g. `+foo+` instead of
`foo`.

As with `defvar` and `defparameter`, constants are globally reachable.
While reevaluating a `defconstant` form is technically possible, the standard
doesn't define the result -- *don't rely on it*.

## Assignment

```
(setf x 10)
```

  * **variable** - `(setf x 10)`
  * **array element** - `(setf (aref a 0) 10)`
  * **hash table** - `(setf (gethash 'key hash) 10)`
  * **slot (named field)** `(setf (field o) 10)`