(in-package :quest)

(declaim (optimize (debug 3)))

(let ((page (merge-pathnames "../templates/register.html" *static-dir*)))
  (defroute register "/register"
    (ecase (request-method*)
      (:post
       (with-params (:post name email password)
         (with-ban-check (real-remote-addr)
           (if (find-user name)
               "That user already exists!"
               (progn (start-session (add-user name email password))
                      (format nil "Welcome, ~A!  You have been registered." name))))))
      (:get (hunchentoot:handle-static-file page)))))
