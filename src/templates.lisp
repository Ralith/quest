(in-package :quest)

(defvar *template-dir* (truename (merge-pathnames "../templates/" (make-pathname :name nil :type nil :defaults (or #.*compile-file-pathname* *load-pathname*)))))

(defun find-template (name)
  (aprog1 (make-pathname :name name :type "tmpl" :defaults *template-dir*)
    (unless (probe-file it)
      (error "Template \"~A\" does not exist!" name))))