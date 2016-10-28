# Macros

## Loops

### dolist
```
(dolist (var <list-form>)
  <body-form*>)
```

### dotimes
```
(dotimes (var <count-form>)
  <body-form>*)
```

*count-form* must evaluate to an integer.

### do

```
(do (var-defs*)
  (end-test-form result-form*)
  statement*)
```

As indicated by stars, some parts can have zero or many forms.
E.g. variable definitions, result forms and statements can be omitted.

```
;; var-def 
(var init-form step-form)
```

Eval order

  1. Per-iter
    1. each loop variable is given its new value
    2. `end-test-form` is evaluated
        * if falsy - eval each `statement` form
        * if truthy - eval each `result-form`
    

  * Looping stops when `end-test-form` is truthy (e.g. non-nil)
  * Once looping stops, `result-form*` are evaluated
    * the value of the last `result-form` is returned

### loop

Loop exists in two forms, the simple loop, and a complex form with its own
mini-DSL.


##### Simple Loop
```
(loop
  body-form*)
```
Note you can break out of the loop using `(return)`.

##### Complex form
```
(loop for i from 1 to 10 summing (+ 2 i))
;; => 75
```

```
(let ((names '("one" "two" "three")))
  (loop for n in names collecting
    (concatenate 'string n "-mississippi")))
;; => ("one-mississippi" "two-mississippi" "three-mississippi")
```

As mentioned `loop` comes with its own DSL for specifying various types of
loops. The examples above ranged from a (simple) eternal loop through
summing up a value (numerical accumulation) to collecting values int a new
list (list accumulation).

See the [hyperspec loop entry](http://www.lispworks.com/documentation/lw51/CLHS/Body/m_loop.htm) for more information.

## Defining Macros

  * **quote** - `'` / `(quote)`
  * **backquote** - `` ` ``  
  * **unquote** - `,`
  * **unquote-splice** - `,@`

##### Backquote vs quote
Backquoting and quoting are two (literally) complementary approaches to
achieving the same result.

With quoting, everything is by default evaluated before being returned, and
quoting escapes evaluation of a form, returning the literal as written.

Conversely, backquoted forms retain their literal form, though nested forms
which should yield their evaluated value can be *un-quoted* using ``,``.

Here's two examples to illustrate:

| List STX             | Backquote STX           | Result           |
| -------------------- | ----------------------- | ---------------- |
| `` `(a (+ 1 2) c)``  | `(list 'a '(+ 1 2) 'c)` | `(a (+ 1 2) c)`  |
| `` `(a ,(+ 1 2) c)`` | `(list 'a (+ 1 2) 'c)`  | `(a 3 c)`        |


##### Nested backquotes
Still haven't figured this out completely. But it's (at least) possible to get
back into a backquoted mode by using a backquote on a form within an unquoted
form, as seen below:
```
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
```


The `once-only` macro allows evaluating incoming macro parameters once only, and
in order.
```
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
```

This macro though, I don't fully understand yet