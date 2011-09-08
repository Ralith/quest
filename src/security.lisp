(in-package #:quest)

(defdao ban ()
    ((address :col-type :inet :initarg :address :accessor address)
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created)
     (duration :col-type (or interval db-null) :initarg :duration :accessor duration)
     (reason :col-type text :initarg :reason :accessor reason))
  (:keys address))

(defmethod print-object ((o content) s)
  (print-unreadable-object (o s :type t)
    (format s ""
            (string-upcase (content-type o))
            (subst nil :null (title o))
            (id o))))

(defmacro with-ban-check (address &body body)
  (with-gensyms (ban)
   `(if-let (,ban (get-dao 'ban ,address))
      (concatenate 'string "You are banned: " (reason ,ban))
      (progn ,@body))))

(defun user> (a b &aux (alevel (level a)) (blevel (level b)))
  (or (and (string= alevel "admin")
           (or (string= blevel "user")
               (string= blevel "mod")))
      (and (string= alevel "mod")
           (string= blevel "user"))))

(defun can-edit? (user content)
  (or (= (id user) (user-id content))
      (user> user (get-dao 'user (user-id content)))
      (string= (level user) "admin")))
