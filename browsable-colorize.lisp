;;;; Copyright 2023 Anton Vodonosov (avodonosov@yandex.ru)
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the “Software”), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.


#|
TODO:
- multiple definitions for the same symbol (e.g. a function and a variable):
  generate a choice HTML page with links to every definition,
  similar to CLHS "Meanings for " pages
  (e.g. "Meanings for LIST" - http://clhs.lisp.se/Body/a_list.htm
  "Meanings for FLOAT" - http://clhs.lisp.se/Body/a_float.htm)
  Currently we just link to the first definition found.
- guess packages of an unqualified name based on in-package, 
  defpackage, etc.
  Postponed. Currently we simply list candidate packages manually.
  Ideal implementation would require not only knowing the
  standard package change forms, but also applying macro-expansion
  in case user defines his own macros for that.
  Another problem - when the "colorize" library calls
  our formatter, we only passed the symbol text, it's
  position within the file is unknown, nor the sourrounding
  form. (At least I don't know currently how to get this info
  from "colorize").
- When mapping from char position to line numbers, we assume
  utf-8. This may miscalculate silently or with error
  for charsets where latin letters occupy multiple bytes,
  or in case national characters are used in the source code
  and encoded differently than utf-8.
  Ideally, support automatic detection, by file header comments
  or/and from the ADSF system that source file belongs to.
  Some manual configuration may be OK too.
|#

(defpackage browsable-colorize
  (:export with-browsable-context)
  (:use cl))

(in-package browsable-colorize)

;; What parameters are needed to find a symbol,
;; to be portable with regard to readtable case
;; (find-symbol (string '#:make-context) '#:cl+ssl)

;; does reader intern symbols? yes (when two colons are used)
;; (read-from-string "cl+ssl::my-test-sym")

;; given a file, can we find which ASDF system it belongs to?
;; (asdf:map-systems (lambda (sys)
;;                     (format t "~A ~A~%" sys
;;                             (asdf:system-source-directory sys))
;;                     ))

;; (swank/backend:find-definitions 'asdf:system-relative-pathname)
;; => (... (:LOCATION (:FILE "/home/anton/my/unpacked/ccl-1.12.1/ccl/tools/asdf.lisp")
;;                    (:POSITION 425905) ...) ...)


;;; mapping from char position reported by swank to line number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *utf-8-external-format* asdf:*utf-8-external-format*
  "See docstring for ASDF:*UTF-8-EXTERNAL-FORMAT*.")

(defparameter *utf-8-compatible-character-type*
  #+lispworks 'lw:simple-char
  #-lispworks 'character)

;; Note:
;; Github uses 1-based line numbers.
;; Swank and emacs use 1-based line and char numbers.

(defun line-positions (file-name)
  "Returns an array whose i-th element is a character position of i-th line
in the specified file. The character positions are one-based.
The line nubmers are zero-based just becasuse that's how arrays are indexed.
Even empty file is assumed to have one line starting with
a noexisting character 1."
  (with-open-file (f file-name
                     :direction :input
                     :element-type *utf-8-compatible-character-type*
                     :external-format *utf-8-external-format*)
    (let ((result (make-array 3000
                              :adjustable t
                              :fill-pointer 0
                              :element-type 'fixnum))
          (pos 1)
          (line 1))
      (vector-push-extend pos result)
      (loop
        (let ((char (read-char f nil 'eof)))
          (when (eq 'eof char)
            ;; (format t "finished~%")
            (return))
          (incf pos)
          (when (char= #\Newline char) ; TODO: or use CHAR-EQUAL? or EQL?
            (incf line)
            ;; (format t "line ~A pos ~A~%" line pos)
            (vector-push-extend pos result))))
      result)))

(defparameter *line-pos-cache* nil)

(defun with-line-pos-cache-impl (body-fn)
  (let ((*line-pos-cache* (make-hash-table :test 'equal)))
    (funcall body-fn)))

(defmacro with-line-pos-cache (&body body)
  `(with-line-pos-cache-impl (lambda () ,@body)))

(defun line-num (file char-pos)
  "Given a 1-based character position in a file,
returns a 1-based line number."
  (let ((positions (or (and *line-pos-cache*
                            (or (gethash file *line-pos-cache*)
                                (setf (gethash file *line-pos-cache*)
                                      (line-positions file))))
                       (line-positions file))))
    ;; TODO: use binary-search instead of linear search?
    (let ((line 0))
      (loop
        (if (> (aref positions line) char-pos)
            ;; We reached the first line whose char position is greater
            ;; than thhe CHAR-POS.
            ;; Since the LINE variable is a zero-based index,
            ;; and we need to return a one-based index,
            ;; not adjustment is needed
            (return line)
            (incf line))))))

#|
(time
 (dotimes (unused 10000)
   (line-num "/home/anton/prj/cl+ssl/cl-plus-ssl/src/streams.lisp" 113)))
;; 11 seconds

(time
 (with-line-pos-cache
   (dotimes (unused 10000)
     (line-num "/home/anton/prj/cl+ssl/cl-plus-ssl/src/streams.lisp" 113))))
;; 0.01 seconds
|#

(defun resolve-symbol (symbol-text-from-colorize fallback-packages)
  "Returs a list of symbols which potentially be designated by
SYMBOL-TEXT-FROM-COLORIZE (unqualified symbol names may be ambigous,
hence returning a list). 

Keywords are treated as references to unqualified symbols.
For example, in a defpakcage form keywords often used
to designate expored / imported non-keyword symbols.

slime-edit-definition only treats uninterned keywords -
like #:something - that way, as references to unqualified
symbols. But for our purposes let's extend that to
normal keywards as well.
                 
The FALLBACK-PACKAGES is a list of package deisgnators
to try in case of unqalified symbols name."
  (let* ((stext symbol-text-from-colorize) ; shorthand
         (last-colon (position #\: stext :from-end t :test #'char=))
         (unqualified-ref (cond ((null last-colon)
                                 stext)
                                ;; A keyword?
                                ((or (= last-colon 0)
                                     ;; uninterned keyword #:something
                                     (and (= last-colon 1)
                                          (char= (aref stext 0) #\#)))
                                 (subseq stext (1+ last-colon))))))
    (if unqualified-ref
        (let (;; apply the readtable case
              (name (string (read-from-string (concatenate nil
                                                           "#:"
                                                           unqualified-ref))))
              (result nil))
          (dolist (pkg fallback-packages)
            (let ((sym (find-symbol name pkg)))
              (when sym
                (setq result (cons sym result)))))
          (nreverse result))
        ;; a qualified symbol
        (list (read-from-string stext)))))

;; The 2 in the name is just because it is the second
;; version. Initially there was another implementation.
;; And it stillexists in an uncommited work file, so I
;; don't want want to introduce collision by removing the 2x.
(defun github-uri2 (swank-definition dir-to-github-alist)
  (let* ((location (cdr (assoc :location swank-definition)))
         (file (second (assoc :file location))))
    (dolist (dir-to-github-pair dir-to-github-alist)
      (let ((rel-path (enough-namestring file (car dir-to-github-pair))))
        (when (< (length rel-path)
                 (length file))
          (return
            (format nil
                    "~A~A#L~A"
                    (cdr dir-to-github-pair)
                    rel-path
                    (line-num file (second (assoc :position location))))))))))

(defun github-uris (symbol dir-to-github-alist)
  (mapcar (lambda (def)
            (github-uri2 def dir-to-github-alist))
          (swank/backend:find-definitions symbol)))

#|

swank/backend:find-definitions is mroe complete than swank/backend:find-source-location:

(defvar mytest 1)
(defmacro mytest () 2)
(swank/backend:find-definitions 'mytest)
;;  => two definitions
(length (swank/backend:find-source-location 'mytest))
;;  => one location

|#

#|
(github-uri2 (first (swank/backend:find-definitions 'cl+ssl:make-context))
             (list (cons (asdf:system-source-directory "cl+ssl")
                         "https://github.com/cl-plus-ssl/cl-plus-ssl/tree/master/")))
|#

;;; the the formatter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fallback-packages* nil)
(defparameter *dir-to-github-alist* nil)

;; The 2 in the name is just because it is the second
;; version. Initially there was another implementation.
;; And it stillexists in an uncommited work file, so I
;; don't want want to introduce collision by removing the 2x.
(colorize::define-coloring-type :common-lisp-browsable "Common Lisp Browsable"
  :parent :common-lisp
  :formatters 
  (((:symbol :escaped-symbol :keyword)
    (lambda (type symbol-text)
      
      (declare (ignore type))
      ;; (format t "~A ~A~%" type symbol-text)

      (let* ((symbols (resolve-symbol symbol-text *fallback-packages*))
             (swank-defs (mapcan #'swank/backend:find-definitions symbols))
             (github-uri (when (plusp (length swank-defs))
                           (github-uri2 (first swank-defs)
                                        *dir-to-github-alist*))))
        (if github-uri
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    github-uri
                    (colorize::call-parent-formatter))
            (colorize::call-parent-formatter)))))))


(defun with-browsable-context-impl (fallback-packages dir-to-github-alist body-fn)
  (let ((*fallback-packages* (mapcar #'find-package fallback-packages))
        (*dir-to-github-alist* dir-to-github-alist))
    (with-line-pos-cache
      (funcall body-fn))))

(defmacro with-browsable-context ((fallback-packages dir-to-github-alist) &body body)
  "
Wrap into this macro the invocations of the COLORIZE library
functions when using them with our :COMMON-LISP-BROWSABLE  coloring type,

FALLBACK-PACKAGES is a list of package designators,
that are tried (in the order specified) to locate unqualified symbols.

DIR-TO-GITHUB-ALIST is an alist mapping from local file system directory
to a base github URI. Used to map source code location found
on local system to the github URLs.
"
  `(with-browsable-context-impl ,fallback-packages
                                ,dir-to-github-alist
                                (lambda () ,@body)))
