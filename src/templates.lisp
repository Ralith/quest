(in-package :quest)

(defparameter *static-dir* (asdf:system-relative-pathname :quest "static/"))

(setf template:*string-modifier* #'identity)
(setf template:*default-template-pathname* (asdf:system-relative-pathname :quest "templates/"))
