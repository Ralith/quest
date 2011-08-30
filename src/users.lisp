(in-package #:quest)

(declaim (optimize (debug 3)))

(defconstant +salt-size+ 32)

(defprepared-with-names %register (name email password)
    ((:insert-into '#:users :set '#:name :$1 '#:email :$2 '#:password :$3 :returning '#:id)
     name email password)
    :single)

(defprepared-with-names username->id (name)
    ((:select '#:id :from '#:users :where (:= '#:name :$1))
     name)
    :single)

(defprepared-with-names password-of (user-id)
    ((:select '#:password :from '#:users :where (:= '#:id :$1))
     user-id)
    :single)

(defprepared-with-names (setf password-of) (password user-id)
    ((:update '#:users :set '#:password :$1 :where (:= '#:id :$2))
     password user-id)
    :none)

(defprepared-with-names user-details (id)
    ((:limit (:select :* :from '#:users :where (:= '#:id :$1))
             1)
     id)
    :row)

(defun register (name email password)
  (nth-value 0 (%register name email (bcrypt:hash password))))

(defun login (name password &aux)
  (let* ((user-id (username->id name))
         (hash (password-of user-id)))
    (if (and hash (bcrypt:password= password hash))
        (progn ;; (start-session)
               ;; (setf (session-value :user)
               ;;       name)
          user-id)
        nil)))
