(in-package :quest)

;;; Random
(defun random-data (length &aux (buffer (make-array length :element-type '(unsigned-byte 8))))
  #+unix
  (with-open-file (s "/dev/urandom" :element-type '(unsigned-byte 8))
    (read-sequence buffer s))
  #-unix (map-into buffer (curry #'random 256)) ; TODO: Better randomness on nonunix
  buffer)

;;; Hunchentoot
(defmacro with-params ((method &rest params) &body body)
  `(let ,(loop for name in params
               collect (list name (ecase method
                                    (:get `(hunchentoot:get-parameter ,(string-downcase (symbol-name name))))
                                    (:post `(hunchentoot:post-parameter ,(string-downcase (symbol-name name)))))))
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
  (map nil (compose #'drop-table #'car) (reverse pomo::*tables*)))
