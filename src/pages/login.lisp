(in-package :quest)

(declaim (optimize (debug 3)))

(let ((page (merge-pathnames "login.html" *template-dir*)))
  (defroute login "/login"
    (with-params (:post name password)
      (if (and name password)
          (if (validate-user name password)
              "Login success!"
              "Invalid credentials")
          (handle-static-file page)))))
