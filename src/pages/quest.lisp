(in-package #:quest)

(define-easy-handler (quest :uri "/quest") (id)
  (when id
    (setf id (parse-integer id :radix 36))
    (let ((chapters
            (loop for (chapter-id title id ordinal post-count chapter-date)
                  in (mapcar #'chapter-details (chapter-ids id))
                  collecting
                  (list :chapter-title title
                        :posts
                        (loop for (pid pau padd ptit pnam pbod pdat ped)
                              in (subst nil :null (mapcar #'post-details (updates-of chapter-id)))
                              collecting (list :post-title ptit
                                               :author pau
                                               :date pdat
                                               :body pbod))))))
      (destructuring-bind (id title author chapter-count created) (quest-details id)
        (declare (ignore id chapter-count created))
       (with-output-to-string (s)
         (fill-and-print-template (find-template "quest") (list
                                                           :quest-title title
                                                           :author author
                                                           :chapters chapters)
                                  :stream s))))))
