# Quickstart
All the stuff not directly covered in
[Practical Common Lisp](http://gigamonkeys.com/book) or elsewhere but immensely
useful when starting out.

## QuickLisp

### Installation
[Download QuickLisp](https://beta.quicklisp.org/quicklisp.lisp).

```
sbcl --load quicklisp.lisp

;; ...
(quicklisp-quickstart:install)

;; ensure quicklisp is automatically loaded on sbcl startup
(ql:add-to-init-file)

(quit)
```
If done right, your SBCL user initialization file (default `~/.sbclrc`) should
reflect that quicklisp will be loaded automatically on start.


### Configuring ASDF (& hence quickload) to scan local lisp projects

create a file, `~/.config/common-lisp/source-registry.conf.d/projects.conf`:
```
(:tree (:home "repos/cl"))
```

### Installing libraries

#### Search for libraries by name
```
(ql:system-apropos "<search term here>")
```

(I suspect it's meant to be remniscient of bash's `apropos` command.)

#### Install/Load Library
Quicklisp will fetch the library and any required dependencies.

```
(ql:quickload "fset")
```

#### Making a new project
These instructions are lifted from [Making a small Lisp project with quickproject & Quicklisp](http://xach.livejournal.com/278047.html).

##### Prerequisites
  * install quicklisp (see above)
  * Ensure ASDF2 scans the directory tree of your CL projects
    * ASDF2: lower-level makefile-style buildtool which quicklisp relies on
    * 

```
(ql:quickload "quickproject")
(quickproject:make-project
  "~/repos/cl/<myproject>"
  :depends-on '(dep1 dep2))
```

### Developing a system

**System**: ASDF(2) terminology for a library/program. An example system,
*yggdrasil*, made by *quickproject*, consists of a few files:

```
.
|-- yggdrasil
    |-- README.txt
    |-- package.lisp
    |-- yggdrasil.asd
    `-- yggdrasil.lisp
```

  * `<project>.lisp` - default file for the system's code - can be renamed, can have more
  * `<project>.asd` - The ASDF file describing the system. Think leiningen project.clj or boot's build.boot. Primarily describes ASDF system dependencies and compilation order for files.
  * `package.lisp` - The file describing the lisp package itself. Describes imports, primarily.

#### &lt;project&gt;.asd
Remniscient of leiningen's project.clj, the asd file is a datastructure naming &
describing the system itself. Specifying its license, author, dependencies on
other systems and compilation order.

Unlike Clojure, there isn't a compilation process wherein a compiler tries to
find leaf nodes in a dependency tree and compiling them first culminating in
the compilation of the "main" file.
Hence, you explicitly specify the order in which lisp files are loaded into
the running image (e.g. ~compiled).

#### package.lisp 
Unlike ASDF(2), the package concept is part of the Common Lisp spec itself.

Hence the root form, `defpackage`, is fully described on this [CLHS page](http://clhs.lisp.se/Body/m_defpkg.htm). 


### Example - make new project & start hacking

```
(ql:quickload "quickproject")
(quickproject:make-project "~/repos/cl/fooproject" :depends-on '(fset))
(ql:quickload "fooproject")
;; open ~/repos/cl/fooproject/fooproject.lisp & start hacking
```

# TODO

  * quicklisp new project (hunchentoot tutorial?)
  * slime features -- see vids, document
  * Find & document unit testing library (e.g. https://github.com/fukamachi/prove )