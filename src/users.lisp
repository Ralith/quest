(in-package #:quest)

(declaim (optimize (debug 3)))

(defdao user ()
    ((id :col-type serial :reader id)
     (name :col-type text :initarg :name :reader name
           :initform (error "Users must be named"))
     (email :col-type text :initarg :email :reader email
            :initform (error "Users must have an email"))
     (iterations :col-type integer :initarg :iterations :reader iterations
                 :initform (error "Users must have a hash iteration count")
                 :documentation "Number of iterations used for PBKDF2-SHA512")
     (salt :col-type bytea :initarg :salt :reader salt
           :initform (error "Users must have a salt"))
     (password :col-type bytea :initarg :password :reader password
               :initform (error "Users must have a password"))
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created))
  (:keys id)
  (:unique name))

(defprepared-with-names find-user (name)
    ((:select :* :from 'user :where (:= 'name '$1))
     name)
    (:dao user :single))

(defconstant +hash-iterations+ 64)
(defun hash-password (password salt &optional (iterations +hash-iterations+)
                      &aux (digest :sha512))
  "Password hashing function."
  (ironclad:derive-key
   (make-instance 'ironclad:pbkdf2 :digest digest)
   (ironclad:ascii-string-to-byte-array password)
   salt
   iterations (ironclad:digest-length digest)))

(defun add-user (name email password &aux (salt (random-data 32)))
  (make-dao 'user :name name :email email
                  :iterations +hash-iterations+
                  :salt salt
                  :password (hash-password password salt +hash-iterations+)))

(defun validate-user (name password &aux (user (find-user name)))
  (when (and user
             (equalp (password user)
                     (hash-password password (salt user) (iterations user))))
      user))
