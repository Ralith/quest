(in-package #:quest)

(defdao ban ()
    ((id :col-type serial :reader id)
     (address :col-type inet :initarg :address :accessor address)
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created)
     (expiration :col-type :timestamp-with-time-zone :col-default "infinity"
                 :initarg :expiration :accessor expiration)
     (reason :col-type text :initarg :reason :accessor reason))
  (:keys id))

(defprepared-with-names banned? (address &optional (timestamp (now)))
    ((:select :* :from 'ban :where (:and (:= 'address :$1)
                                         (:> 'expiration :$2)))
     address timestamp)
    (:dao ban))

(defun timestamp-infinity? (timestamp)
  ;;; Experimentally derived values; probably fragile.
  (and (= (day-of timestamp) 106751931)
       (= (sec-of timestamp) 14454)
       (= (nsec-of timestamp) 775807000)))

(defmethod print-object ((o ban) s)
  (print-unreadable-object (o s :type t)
    (format s "~A (~:[until ~A~;forever~])"
            (address o)
            (timestamp-infinity? (expiration o))
            (expiration o))))

(defmacro with-ban-check (address &body body)
  (with-gensyms (ban)
    `(if-let (,ban (banned? ,address))
         (concatenate 'string "You are banned: " (reason ,ban))
         (progn ,@body))))

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
