(in-package #:quest)

(defclass quest-acceptor (hunchentoot:acceptor) ())

(defmethod hunchentoot:session-cookie-name ((acceptor quest-acceptor))
  "quest.sid")

(defun init-db ()
  (with-transaction (initialization)
    (execute (:create-enum :user_level
                           ("admin" "mod" "user")))
    (execute (:create-enum :content_type
                           ("board" "quest" "chapter" "discussion" "update" "suggestion" "post")))
    (create-all-tables)
    (execute "ALTER TABLE media ADD CONSTRAINT media_content_content_id_foreign FOREIGN KEY (content_id) REFERENCES content (id) ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE")))

(defun start ()
  (hunchentoot:start (make-instance 'quest-acceptor :port 8080))
  (connect-toplevel "quest" "ralith" nil "localhost"))

(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf8))
