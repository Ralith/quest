(in-package #:quest)

(defdao ban ()
    ((id :col-type serial :reader id)
     (address :col-type inet :initarg :address :accessor address)
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created)
     (expiration :col-type (or :timestamp-with-time-zone db-null) :initarg :expiration :accessor expiration)
     (reason :col-type text :initarg :reason :accessor reason))
  (:keys id))

(defmethod print-object ((o content) s)
  (print-unreadable-object (o s :type t)
    (format s ""
            (string-upcase (content-type o))
            (subst nil :null (title o))
            (id o))))

(defmacro with-ban-check (address &body body)
  (with-gensyms (ban)
   `(let ((,ban (get-dao 'ban ,address)))
      (if (and ,ban (timestamp> (expiration ,ban) (now)))
          (concatenate 'string "You are banned: " (reason ,ban))
          (progn ,@body)))))

(defun user-level> (a b)
  (or (and (string= a "admin")
           (or (string= b "mod")
               (user-level> "mod" b)))
      (and (string= a "mod")
           (string= b "user"))))

(defun user> (a b)
  (user-level> (level a) (level b)))

(defun can-edit? (user content)
  (or (= (id user) (user-id content))
      (user> user (get-dao 'user (user-id content)))
      (string= (level user) "admin")))
