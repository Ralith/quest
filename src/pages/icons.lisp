(in-package :quest)

(defparameter *favicon* (merge-pathnames "favicon.ico" *static-dir*))

(defun icon-dispatcher (request &aux (path (hunchentoot:script-name request)))
  (cond
    ((string= "/favicon.ico" path)
     (hunchentoot:handle-static-file (hunchentoot:handle-static-file *favicon*)))))

(pushnew 'icon-dispatcher hunchentoot:*dispatch-table*)
