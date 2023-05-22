(in-package browsable-colorize)

(defun patch-css (coloring-css)
  (restart-case 
      (let* ((class ".string")
             (start (search class coloring-css))
             (end (search "}" coloring-css
                          :start2 (or start
                                      (error 
                                       "The ~A class is not found in
 colorize:*coloring-css*, probably the version of colorize curretly
 in use has changed the CSS classes so the colors of the resulting
 document will not match our expectation. In particular,
 the string literal color we try to override will not be overriden.
 Use CONTINUE restart to proceed with unpatched colorize:*coloring-css."
                                       class)))))
        (concatenate 'string
                     (subseq coloring-css 0 start)
                     ".string { color : #0a3069; background-color : inherit; }"
                     ;; ".string { color : black; background-color : inherit; }"
                     (subseq coloring-css
                             (1+ end))))
    (continue ()
      :report "Use the unpatched colorize:*coloring-css*"
      coloring-css)))

;; (patch-css colorize:*coloring-css*)
;; (patch-css "abc")

(defun better-css ()
  (patch-css colorize:*coloring-css*))
