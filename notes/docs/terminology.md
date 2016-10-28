# Terminology

## Overview
A (common) lisp environment consists of two parts, a *reader* and an *evaluator*.

The **reader** turns strings of characters into s-expressions.

The **evaluator** defines the a syntax of lisp *forms*. **NOTE**, not all s-expressions
are valid lisp forms. E.g. `("hello" 1)` -- is invalid because the first element of the list isn't a symbol.


### S-Expressions

S-expressions are subdivided into two types:

1. lists (e.g. delimited by parentheses)
2. atoms (e.g. everything else)

*Technically* `nil` is the only "thing" which is both an atom **and** a list - nil is equivalent to the empty list. (I.e. `(list)`, `'()`, `nil` are the same).

### (Lisp) Forms
Lisp forms are what's evaluated by the *evaluator*.

The (syntax) rule for determining valid forms:

  * any atom (e.g. any non-list / the empty list)
  * any list whose first element is a symbol

### Symbols

Examples: `*db*`, `format`, `dolist` (actually a variable, a function and a macro, respectively)

**NOTE**: CL is a Lisp-2 (e.g. separate variable and function namespaces) so to disambiguiate, `#'` is prefixed symbols which are to be interpreted as coming from the function namespace.
I.e., `list` is a symbol referencing a variable and `#'list` is a symbol referencing a function.

## Evaluation Rules

Evaluation can be divided into 3 categories; *functions*, *special operators* and *macros*.

### Functions
All arguments following the function symbol are evaluated, in-order, before their values
are passed to the function itself.

Because they are evaluated, each argument *must be a valid lisp form*!

### Special Operators
Special operators are evaluated according to their unique logic. E.g. `if` have different evaluation logic than `quote` (`'`) do.

CL has about 25 special operators in total, though a large amount of these are somewhat esoteric and can be ignored.

### Macros

Macros are evaluated in **2** steps
  
  1. raw, unevaluated s-expr's are passed to the macro as input args
  2. (**macro expansion**) output returned by macro is evaluated according to normal evaluation rules
    * This means macros don't recursively expand calls to other macros
    * Resulting form is evaluated, other referenced macros are thus evaluated in turn etc.

To see the result of the first step of macro evaluation, use `macroexpand-1`, e.g:

```
(defmacro kw (kw)
  "create keyword value from input.
  E.g. (kw sol) => :sol"
  (intern (symbol-name kw) "KEYWORD"))

(macroexpand-1 '(kw sol))
```

(in this case, there's nothing left to do in step 2, but at times, e.g. when referencing other  macros or when outputting a  list such as `(+ 1 2)` you would see the form prior to its evaluation.)

####Why macros are cool
1. implement constructs with custom evaluation logic (e.g. short-circuiting as in `if`)
2. (comparatively) easy-to-use code generation
3. macros do all their pre-step-2 work at **compile-time** - e.g. it's possible to do a lot
   of heavy lifting (creating clever code) once without paying for it at run-time.
     * (Simplistic example: precompute prime number table)


## Equality / Truthyness

  * `eq` -- implementation-specific object-equality, don't use
  * `eql` -- 'eq' + objects of same class representing same
	        number/character value guaranteed to be true
  * `equal` -- 'eql' + objects with same structure & contents (recursively)
	  are considered equal.
  * `equalp` -- 'equal' + string-insensitive equivalence

## Destructive Functions
For reasons of efficiency, certain functions are allowed to modify
elements in place, more or less arbitrarily - *destroying* the input argument
in the process.

Two implications:

  1. always store the *output* of the function somewhere
  2. never attempt to re-use what was passed into the function.

E.g. `(sort <seq> <compare-fn>)` is one such function.