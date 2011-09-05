(in-package #:quest)

(defroute quest "/quest/:id"
  (let* ((quest (get-dao 'quest (parse-integer id :radix 36)))
         (chapters
           (loop for chapter in (chapters quest)
                 collecting
                 (list :chapter-title (title chapter)
                       :updates
                       (loop for update in (updates-of chapter)
                             for post = (get-dao 'post (post-id update))
                             collecting (list :update-title (title post)
                                              :author (name (get-dao 'user (user-id post)))
                                              :date (created post)
                                              :body (body post)))))))
    (with-output-to-string (s)
      (fill-and-print-template (find-template "quest")
                               (list :quest-title (title quest) 
                                     :author (name (get-dao 'user (user-id quest)))
                                     :chapters chapters)
                               :stream s))))
