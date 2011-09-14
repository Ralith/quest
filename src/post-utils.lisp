(in-package :quest)

(defvar *media-dir* (asdf:system-relative-pathname :quest "media/"))

(defun media-path (media)
  (merge-pathnames
   (with-output-to-string (s)
     (map 'nil (rcurry 'write :stream s :base 16) (checksum media)))
   *media-dir*))

(defun store-media (content path file-name content-type
                    &aux (checksum (ironclad:digest-file :sha512 path)))
  (aprog1 (make-dao 'media
                    :content-id (id content)
                    :content-type content-type
                    :name file-name
                    :checksum checksum)
    (let ((new-path (media-path it)))
      (unless (probe-file new-path)
        (copy-file path new-path)))))

(defun make-quest (author address board
                   &key (alias :null)
                     (quest-title "???") (quest-thumbnail :null) (quest-summary :null) quest-media
                     (chapter-title :null) (chapter-thumbnail :null) (chapter-summary :null) chapter-media
                     (update-title :null) (update-thumbnail :null) (update-body :null) update-media)
  (with-transaction (make-quest)
    (let* ((quest (make-dao 'quest
                            :parent-id (id board)
                            :ordinal (alloc-ordinal board)
                            :user-id (id author)
                            :address address
                            :title quest-title
                            :alias alias
                            :thumbnail quest-thumbnail
                            :body quest-summary))
           (chapter (make-dao 'chapter
                              :parent-id (id quest)
                              :ordinal (alloc-ordinal quest)
                              :user-id (id author)
                              :title chapter-title
                              :alias alias
                              :thumbnail chapter-thumbnail
                              :body chapter-summary))
           (update (make-dao 'update
                             :parent-id (id chapter)
                             :ordinal (alloc-ordinal chapter)
                             :user-id (id author)
                             :address address
                             :title update-title
                             :alias alias
                             :thumbnail update-thumbnail
                             :body update-body)))
      (make-dao 'discussion
                :parent-id (id quest)
                :user-id (id author)
                :address address)
      (let ((media ())
            (complete nil))
        (unwind-protect
             (flet ((store-func (content)
                      (curry (lambda (x) (push x media))
                             (curry 'apply 'store-media content))))
               (mapc (store-func quest) quest-media)
               (mapc (store-func chapter) chapter-media)
               (mapc (store-func update) update-media)
               (setf complete t))
          (unless complete
            (mapc (compose 'delete-file 'media-path) media)
            (abort-transaction make-quest)))))))
