(in-package :quest)

;;; Hunchentoot
(defmacro with-params ((method &rest params) &body body)
  `(let ,(loop for name in params
               collect (list name (ecase method
                                    (:get `(get-parameter ,(string-downcase (symbol-name name))))
                                    (:post `(post-parameter ,(string-downcase (symbol-name name)))))))
     ,@body))

;;; SQL utils
(defmacro defdao (name superclasses slots &body dao-options)
  (flet ((parsed-opts (keyword deftable-func-name)
           (mapcar (lambda (statement)
                     `(,deftable-func-name ,@(mapcar (curry #'list 'quote) (cdr statement))))
                   (remove-if-not (lambda (opt) (eq keyword (car opt)))
                                  dao-options))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         (:metaclass dao-class)
         ,@(when-let (keys (assoc :keys dao-options)) `(,keys)))
       (deftable ,name
         (!dao-def)
         ,@(parsed-opts :index '!index)
         ,@(parsed-opts :unique-index '!unique-index)
         ,@(parsed-opts :foreign-key '!foreign)
         ,@(parsed-opts :unique '!unique)))))

(defun drop-table (symbol)
  (query (format nil "drop table if exists ~A" (sql-compile symbol))))
(defun drop-all-tables ()
  (map nil (compose #'drop-table #'car) pomo::*tables*))
