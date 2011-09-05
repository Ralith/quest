(in-package :postmodern)

(defun !unique (&rest target-fields)
  (format nil "ALTER TABLE ~A ADD CONSTRAINT ~A UNIQUE (~{~A~^, ~})"
          (to-sql-name *table-name*)
          (to-sql-name (format nil "~A_~{~A~^_~}_unique" *table-name* target-fields))
          (mapcar #'pomo::to-sql-name target-fields)))
(export '!unique (find-package :postmodern))