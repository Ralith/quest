(in-package #:quest)

(declaim (optimize (debug 3)))

(defconstant +salt-size+ 32)

;;; Not authoritative for schema due to lack of constraint support
;; (defclass users ()
;;   ((id :col-type :serial
;;        :reader id)
;;    (name :col-type :string :initarg :name
;;          :reader name)
;;    (email :col-type :string :initarg :email
;;          :reader email)
;;    (password :col-type :string :initarg :password
;;              :reader password)
;;    (created :col-type :timestamp-with-timezone
;;             :reader created))
;;   (:metaclass dao-class)
;;   (:keys id))

(defprepared %register
    (:insert-into '#:users :set '#:name :$1 '#:email :$2 '#:password :$3 :returning '#:id)
    :single)

(defprepared id-of-user
    (:select '#:id :from '#:users :where (:= '#:name :$1))
    :single)

(defprepared password-of
    (:select '#:password :from '#:users :where (:= '#:id :$1))
    :single)

(defprepared (setf password-of)
    (:update '#:users :set '#:password :$1 :where (:= '#:id :$2))
    :none)

(defun register (name email password)
  (nth-value 0 (%register name email (bcrypt:hash password))))

(defun login (name password &aux)
  (let* ((user-id (id-of-user name))
         (hash (password-of user-id)))
    (if (and hash (bcrypt:password= password hash))
        (progn ;; (start-session)
               ;; (setf (session-value :user)
               ;;       name)
          user-id)
        nil)))
