(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: Most recently updated
(defprepared-with-names frontpage-quests (count)
    ((:limit (:select :* :from 'content :where (:= 'type "quest")) :$1) count)
    (:dao content))

(defroute frontpage "/"
  (let ((values (loop for quest in (frontpage-quests 10)
                      for chapter = (latest-chapter quest)
                      collecting
                      `(:quest-title ,(title quest)
                        :quest-id ,(write-to-string (id quest) :base 36)
                        :chapter-title ,(title chapter)
                        :posts
                        ,(loop for update in (updates chapter)
                               collecting `(:post-title ,(title update)
                                            :author ,(name (get-dao 'user (user-id update)))
                                            :date ,(created update)
                                            :body ,(body update)))))))
    (with-output-to-string (s)
      (fill-and-print-template (find-template "index")
                               (list
                                :user-name (let ((session (hunchentoot:session hunchentoot:*request*)))
                                             (and session (name (get-dao 'user (user-id session)))))
                                :quests values)
                               :stream s))))

(defun startup ()
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080))
  (connect-toplevel "quest" "ralith" nil "localhost"))