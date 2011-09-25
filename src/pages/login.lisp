(in-package :quest)

(declaim (optimize (debug 3)))

(let ((page (merge-pathnames "../templates/login.html" *static-dir*)))
  (defroute login "/login"
    (ecase (request-method*)
      (:post
       (with-params (:post name password)
         (if-let (user (validate-user name password))
           (progn (start-session user)
                  (hunchentoot:redirect (hunchentoot:referer)
                                        :code hunchentoot:+http-see-other+))
           "Invalid credentials")))
      (:get (hunchentoot:handle-static-file page)))))

(defroute logout "/logout"
  (if (hunchentoot:session hunchentoot:*request*)
      (progn (end-session)
             (hunchentoot:redirect (hunchentoot:referer)
                                   :code hunchentoot:+http-see-other+))
      "You're not logged in!"))
