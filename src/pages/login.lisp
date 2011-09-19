(in-package :quest)

(declaim (optimize (debug 3)))

(let ((page (merge-pathnames "login.html" *template-dir*)))
  (defroute login "/login"
    (ecase (request-method*)
      (:post
       (with-params (:post name password)
         (if-let (user (validate-user name password))
           (progn (start-session user)
                  "Login success!")
           "Invalid credentials")))
      (:get (hunchentoot:handle-static-file page)))))

(defroute logout "/logout"
  (if (hunchentoot:session hunchentoot:*request*)
      (progn (end-session)
             "Logged out.")
      "You're not logged in!"))
