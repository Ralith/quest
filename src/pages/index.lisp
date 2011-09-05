(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: Most recently updated
(defprepared-with-names frontpage-quests (count)
    ((:limit (:select :* :from 'quest) :$1) count)
    (:dao quest))

(defroute frontpage "/"
  (let ((values (loop for quest in (frontpage-quests 10)
                      for chapter = (latest-chapter quest)
                      collecting
                      `(:quest-title ,(title quest)
                        :quest-id ,(write-to-string (id quest) :base 36)
                        :chapter-title ,(title chapter)
                        :posts
                        ,(loop for update in (updates-of chapter)
                               for post = (get-dao 'post (post-id update))
                               collecting `(:post-title ,(title post)
                                            :author ,(name (get-dao 'user (user-id post)))
                                            :date ,(created post)
                                            :body ,(body post)))))))
    (with-output-to-string (s)
      (fill-and-print-template (find-template "index") (list :quests values)
                               :stream s))))

(defun startup ()
  (start (make-instance 'hunchentoot:acceptor :port 8080))
  (connect-toplevel "quest" "ralith" nil "localhost"))