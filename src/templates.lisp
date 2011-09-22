(in-package :quest)

(defparameter *template-dir* (asdf:system-relative-pathname :quest "templates/"))
(defparameter *static-dir* (asdf:system-relative-pathname :quest "static/"))

(defun find-template (name)
  (aprog1 (make-pathname :name name :type "tmpl" :defaults *template-dir*)
    (unless (probe-file it)
      (error "Template \"~A\" does not exist!" name))))