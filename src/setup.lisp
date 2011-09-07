(in-package #:quest)

(defparameter *root-url* "http://localhost:8080/")

(defclass quest-acceptor (hunchentoot:acceptor) ())

(defmethod hunchentoot:session-cookie-name ((acceptor quest-acceptor))
  "quest.sid")

(defun init-db ()
  (execute (:create-enum :content_type
                         ("board" "quest" "chapter" "discussion" "update" "suggestion" "post")))
  (create-all-tables))

(defun start ()
  (hunchentoot:start (make-instance 'quest-acceptor :port 8080))
  (connect-toplevel "quest" "ralith" nil "localhost"))
