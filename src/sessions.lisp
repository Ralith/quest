(in-package :quest)

;;; Days
(defparameter +session-timeout+ 7)

(defdao session ()
    ((id :col-type bytea :reader id :initform (random-data 16))
     (user-id :col-type integer :initarg :user-id :reader user-id)
     (remote-addr :col-type inet
                  :initform (hunchentoot:real-remote-addr hunchentoot:*request*)
                  :reader remote-addr)
     (last-activity :col-type :timestamp-with-time-zone :col-default (:now)
                    :accessor last-activity))
  (:keys id)
  (:foreign-key user user-id id))

(defmethod print-object ((o session) s)
  (print-unreadable-object (o s :type t)
    (usb8-array-to-base64-stream (id o) s :uri t)))

(defmethod hunchentoot:session-cookie-value ((session session))
  (usb8-array-to-base64-string (id session) :uri t))

(defmethod hunchentoot:session-verify ((request hunchentoot:request))
  (let* ((id (or (hunchentoot:cookie-in (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                                        request)
                 (hunchentoot:get-parameter (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                                            request)))
         (session (and id (get-dao 'session (base64-string-to-usb8-array id :uri t))))
         (now (universal-to-timestamp (get-universal-time))))
    (when session
      (if (and (timestamp> (timestamp+ (last-activity session) +session-timeout+ :day)
                           now)
               (string= (real-remote-addr request)
                        (remote-addr session)))
          (progn (setf (last-activity session) now)
                 (update-dao session)
                 session)
          (progn (delete-dao session)
                 nil)))))

(defun start-session (user)
  (unless (boundp 'hunchentoot:*request*)
    (error "Cannot start a session outside of a request"))
  (when-let (current-session (hunchentoot:session hunchentoot:*request*))
    (if (= (user-id current-session)
           (id user))
        (return-from start-session current-session)
        (end-session)))
  (let ((session (make-dao 'session :user-id (id user))))
    (setf (hunchentoot:session hunchentoot:*request*) session
          hunchentoot:*session* session)
    (hunchentoot:set-cookie (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                            :value (hunchentoot:session-cookie-value session))))

(defun end-session (&optional session)
  (unless (boundp 'hunchentoot:*request*)
    (error "Cannot end a session outside of a request"))
  (unless session
    (setf session (hunchentoot:session hunchentoot:*request*)))
  (when session
    (delete-dao session)
    (setf hunchentoot:*session* nil
          (hunchentoot:session hunchentoot:*request*) nil)
    (hunchentoot:set-cookie (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                            :expires 0)))
