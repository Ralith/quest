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
      (template:fill-and-print-template
       (find-template "quest")
       (list :quest-title (title quest) 
             :author (name (get-dao 'user (user-id quest)))
             :chapters chapters)
       :stream s))))

(defroute quest "/quest/:id/feed"
  (let* ((quest (get-dao 'quest (parse-integer id :radix 36)))
         (base-url (format nil "http://~A/quest/~A" (host) id))
         (link (format nil "~A/feed" base-url)))
    (setf (content-type*) "application/rss+xml")
    (with-html-output-to-string (s)
      (:rss :version "2.0" :|xmlns:atom| "http://www.w3.org/2005/Atom"
            (:channel (:title (esc (title quest)))
                      (:link (esc link))
                      (:|atom:link| :href (escape-string link) :rel "self" :type "application/rss+xml")
                      (when (stringp (body quest))
                        (htm (:description (esc (body quest)))))
                      (loop for update in (content-subtree-desc quest 1)
                            do (htm (:item (when (stringp (title update))
                                             (htm (:title (esc (title update)))))
                                           (:link (fmt "~A#~A" base-url
                                                       (write-to-string (id update) :base 36)))
                                           (when (stringp (body update))
                                             (htm (:description (esc (body update)))))))))))))
