(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: Most recently updated
(defprepared frontpage-quests
    (:limit (:select :* :from '#:quests) :$1))

(define-easy-handler (frontpage :uri "/") (n)
  (if n
      (setf n (parse-integer n))
      (setf n 10))
  (let ((values (loop for (id title author chapter-count quest-date) in (frontpage-quests n)
                      for (chapter-id chapter-title quest ordinal post-count chapter-date) = (chapter-details (latest-chapter-id id))
                      collecting
                      `(:quest-title ,title
                        :quest-id ,(write-to-string id :base 36)
                        :chapter-title ,chapter-title
                        :posts
                        ,(loop with updates = (updates-of chapter-id)
                               for (pid pau padd ptit pnam pbod pdat ped)
                               in (subst nil :null (mapcar #'post-details updates))
                               collecting `(:post-title ,ptit
                                            :author ,(userid->name pau)
                                            :date ,pdat
                                            :body ,pbod))))))
    (with-output-to-string (s)
      (fill-and-print-template (find-template "index") (list :quests values)
                               :stream s))))

(defun startup ()
  (start (make-instance 'hunchentoot:acceptor :port 8080))
  (connect-toplevel "quest" "ralith" nil "localhost"))