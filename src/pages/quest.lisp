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

(defroute quest-feed "/quest/:id/feed"
  (let* ((quest (get-dao 'quest (parse-integer id :radix 36)))
         (base-url (format nil "http://~A/quest/~A" (host) id)))
    (setf (content-type*) "application/rss+xml")
    (cxml-xmls:map-node
     (cxml:make-octet-stream-sink (send-headers))
     `("rss"
       (("version" "2.0"))
       ("channel"
        ()
        ("title" () ,(escape-for-html (title quest)))
        ("link" () ,(escape-for-html (format nil "~A/feed" base-url)))
        ("description" () ,(if (stringp (body quest))
                               (escape-for-html (body quest))
                               (format nil "Updates to ~A" (title quest))))
        ;; TODO: Last update to quest, not last edit of it itself.
        ("lastBuildDate" () ,(escape-for-html (to-rfc1123-timestring (last-modified quest))))
        ("generator" () "Quest")
        ,@(loop for chapter in (chapters quest)
                for chapter-number from 1 append
                `(("item"
                   ()
                   ("title" () ,(if (stringp (title chapter))
                                    (escape-for-html (title chapter))
                                    (format nil "Chapter ~D" chapter-number)))
                   ("link" () ,(escape-for-html (format nil "~A#c~D" base-url chapter-number)))
                   ("description" () ,(if (stringp (body chapter))
                                          (escape-for-html (body chapter))
                                          (format nil "The ~:R chapter of ~A"
                                                  chapter-number (title quest))))
                   ("pubDate" () ,(escape-for-html (to-rfc1123-timestring (created chapter)))))
                  ,@(loop for update in (updates chapter)
                          for update-number from 1 collect
                          `("item"
                            ()
                            ("title" () ,(if (stringp (title update))
                                             (escape-for-html (title update))
                                             (format nil "Chapter ~D, update ~D"
                                                     chapter-number update-number)))
                            ("link" () ,(escape-for-html (format nil "~A#c~Du~D"
                                                                 base-url chapter-number update-number)))
                            ("description" () ,(if (stringp (body update))
                                                   (escape-for-html (body update))
                                                   (format nil "The ~:R update of the ~:R chapter of ~A"
                                                           update-number chapter-number (title quest))))
                            ("pubDate" () ,(escape-for-html (to-rfc1123-timestring (created update))))))))))
     :include-namespace-uri nil)))
