(in-package :quest)

(declaim (optimize (debug 3)))

(let ((page (merge-pathnames "register.html" *template-dir*)))
  (defroute register "/register"
    (with-params (:post name email password)
      (if (and name email password)
          (if (find-user name)
              "That user already exists!"
              (progn (add-user name email password)
                     (format nil "Welcome, ~A!  You have been registered." name)))
          (handle-static-file page)))))
