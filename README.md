Helps to turn your package.lisp into an HTML API reference,
by generating a colorized version of it, with every symbol
being a link to its definition at github.

Uses swank/backend:find-definitions to determine source code location
of symbol definitions and [colorize](https://github.com/kingcons/colorize).

# Usage

This library defines new coloring type `:common-lisp-browsable` for the
colorize library. This coloring type is inherited from the original
`:common-lisp` type, only enhances it with linking symbols to their
source code definitions at github. Usage of this coloring type
requires wrapping of colorize invocations into the
`browsable-colorize:with-browsable-context` macro. Of course,
in order to determine source code locations, the code
being processed must be loaded first.


```common-lisp

;; not in Quicklisp yet:
(pushnew "/path/to/browsable-colorize/" asdf:*central-registry* :test #'equal)

(ql:quickload "browsable-colorize")

;; load the library whose package.lisp you want to process
(ql:quickload "cl+ssl")


(browsable-colorize:with-browsable-context
    (;; a list of package designators to try when locating
     ;; unqualified symbols
     '(#:cl+ssl #:cl+ssl/config)
     ;; an alist mapping from local source code directories
     ;; to base github URI
     (list (cons (asdf:system-source-directory "cl+ssl")
                 "https://github.com/cl-plus-ssl/cl-plus-ssl/tree/master/")))
  
  (colorize:colorize-file :common-lisp-browsable
                          (asdf:system-relative-pathname :cl+ssl
                                                         "src/package.lisp"))
  (colorize:colorize-file :common-lisp-browsable
                          (asdf:system-relative-pathname :cl+ssl
                                                         "src/config.lisp")))
```

Usage from GitHub Actions:
https://github.com/cl-plus-ssl/cl-plus-ssl/blob/master/.github/workflows/api-doc.yml
It generates and publishes the following docs:
  - https://cl-plus-ssl.github.io/cl-plus-ssl/package.html
  - https://cl-plus-ssl.github.io/cl-plus-ssl/config.html

# Further Thoughts

Ideally, github should support "go to definition" for Common Lisp out of box.
Or for all programming languages, for example by supporting something like
Emacs TAGS files.

Investigating deeper, we see that gitlab supports Language Server Index Format:
https://docs.gitlab.com/ee/user/project/code_intelligence.html

So, maybe some gitlab user can move in this direction. Utilize swank,
maybe also some reader library whose read results include file
locations of the read objects, and generate LSIF files.

Hopefully, github wiill catch-up.

