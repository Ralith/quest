(in-package #:quest)

(defroute quest "/quest/:id"
  (let* ((quest (get-dao 'quest (parse-integer id :radix 36)))
         (chapters
           (loop for chapter in (chapters quest)
                 collecting
                 (list :chapter-title (title chapter)
                       :updates
                       (loop for update in (updates chapter)
                             collecting (list :update-title (title update)
                                              :author (name (get-dao 'user (user-id update)))
                                              :date (created update)
                                              :body (body update)))))))
    (with-output-to-string (s)
      (fill-and-print-template (find-template "quest")
                               (list :quest-title (title quest) 
                                     :author (name (get-dao 'user (user-id quest)))
                                     :chapters chapters)
                               :stream s))))
