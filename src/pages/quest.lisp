(in-package #:quest)

(defroute quest "/quest/:id"
  ;; TODO: Don't hardcode ID of board
  (let ((quest (find-child (get-dao 'board 1) (parse-integer id :radix 36))))
    (with-output-to-string (s)
      (template:fill-and-print-template
       #p"quest.tmpl"
       (list :quest-title (escape-for-html (title quest))
             :quest-summary (escape-for-html (body quest))
             :author (escape-for-html (name (get-dao 'user (user-id quest))))
             :chapters (loop for chapter in (chapters quest)
                             collecting
                             (list :chapter-title (escape-for-html (title chapter))
                                   :chapter-ordinal (ordinal chapter)
                                   :chapter-summary (escape-for-html (body chapter)))))
       :stream s))))

(defroute quest-feed "/quest/:id/feed"
  ;; TODO: Don't hardcode ID of board
  (let* ((quest (find-child (get-dao 'board 1) (parse-integer id :radix 36)))
         (base-url (format nil "http://~A/quest/~A" (host) id)))
    (xmls:write-xml
     `("rss"
       (("version" "2.0"))
       ("channel"
        ()
        ("title" () ,(escape-for-html (title quest)))
        ("link" () ,(escape-for-html (format nil "~A/feed" base-url)))
        ("description" () ,(if (stringp (body quest))
                               (escape-for-html (body quest))
                               (format nil "Updates to ~A" (escape-for-html (title quest)))))
        ;; TODO: Last update to quest, not last edit of it itself.
        ("lastBuildDate" () ,(escape-for-html (to-rfc1123-timestring (last-modified quest))))
        ("generator" () "Quest")
        ,@(loop for chapter in (chapters quest)
                for chapter-number from 1
                for link = (escape-for-html (format nil "~A#~D" base-url chapter-number))
                append
                `(("item"
                   ()
                   ("title" () ,(if (stringp (title chapter))
                                    (escape-for-html (title chapter))
                                    (format nil "Chapter ~D" chapter-number)))
                   ("link" () ,link)
                   ("guid" () ,link)
                   ("description" () ,(if (stringp (body chapter))
                                          (escape-for-html (body chapter))
                                          (format nil "The ~:R chapter of ~A"
                                                  chapter-number (escape-for-html (title quest)))))
                   ("pubDate" () ,(escape-for-html (to-rfc1123-timestring (created chapter)))))
                  ,@(loop for update in (updates chapter)
                          for update-number from 1
                          for link = (escape-for-html (format nil "~A#c~Du~D"
                                                              base-url chapter-number update-number))
                          collect
                          `("item"
                            ()
                            ("title" () ,(if (stringp (title update))
                                             (escape-for-html (title update))
                                             (format nil "Chapter ~D, update ~D"
                                                     chapter-number update-number)))
                            ("link" () ,link)
                            ("guid" () ,link)
                            ("description" () ,(if (stringp (body update))
                                                   (escape-for-html (body update))
                                                   (format nil "The ~:R update of the ~:R chapter of ~A"
                                                           update-number chapter-number (title quest))))
                            ("pubDate" () ,(escape-for-html (to-rfc1123-timestring (created update))))))))))
     (client-stream "application/rss+xml"))))
