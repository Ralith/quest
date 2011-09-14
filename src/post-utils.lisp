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

(defun store-media-set (transaction content media)
  (let ((stored ())
        (complete nil))
    (unwind-protect
         (progn
           (mapc (curry (lambda (x) (push x stored))
                        (curry 'apply 'store-media content))
                 media)
           (setf complete t))
      (unless complete
        (mapc (compose 'delete-file 'media-path) stored)
        (abort-transaction transaction)))))

(defun %make-quest (transaction author address board &key (alias :null) (title "???") (thumbnail :null) (summary :null) media)
  (aprog1 (make-dao 'quest
                    :parent-id (id board)
                    :ordinal (alloc-ordinal board)
                    :user-id (id author)
                    :address address
                    :title title
                    :alias alias
                    :thumbnail thumbnail
                    :body summary)
    (store-media-set transaction it media)
    (make-dao 'discussion
              :parent-id (id it)
              :user-id (id author)
              :address address)))

(defun %make-chapter (transaction author address quest &key (alias :null) (title :null) (thumbnail :null) (summary :null) media)
  (store-media-set transaction
                   (make-dao 'chapter
                             :parent-id (id quest)
                             :ordinal (alloc-ordinal quest)
                             :user-id (id author)
                             :address address
                             :title title
                             :alias alias
                             :thumbnail thumbnail
                             :body summary)
                   media))

(defun %make-update (transaction author address chapter &key (alias :null) (title :null) (thumbnail :null) (body :null) media)
  (store-media-set transaction
                   (make-dao 'update
                             :parent-id (id chapter)
                             :ordinal (alloc-ordinal chapter)
                             :user-id (id author)
                             :address address
                             :title title
                             :alias alias
                             :thumbnail thumbnail
                             :body body)
                   media))

(defun make-quest (author address board
                   &key (alias :null)
                     (quest-title "???") (quest-thumbnail :null) (quest-summary :null) quest-media
                     (chapter-title :null) (chapter-thumbnail :null) (chapter-summary :null) chapter-media
                     (update-title :null) (update-thumbnail :null) (update-body :null) update-media)
  (with-transaction (trans)
    (let* ((quest (%make-quest trans author address board
                               :alias alias
                               :title quest-title
                               :thumbnail quest-thumbnail
                               :summary quest-summary
                               :media quest-media))
           (chapter (%make-chapter trans author address quest
                                   :alias alias
                                   :title chapter-title
                                   :thumbnail chapter-thumbnail
                                   :summary chapter-summary
                                   :media chapter-media)))
      (%make-update trans author address chapter
                    :alias alias
                    :title update-title
                    :thumbnail update-thumbnail
                    :body update-body
                    :media update-media))))

(defun append-chapter (author address quest
                       &key (alias :null)
                         (chapter-title :null) (chapter-thumbnail :null) (chapter-summary :null) chapter-media
                         (update-title :null) (update-thumbnail :null) (update-body :null) update-media)
  (with-transaction (trans)
    (let* ((chapter (%make-chapter trans author address quest
                                   :alias alias
                                   :title chapter-title
                                   :thumbnail chapter-thumbnail
                                   :summary chapter-summary
                                   :media chapter-media)))
      (%make-update trans author address chapter
                    :alias alias
                    :title update-title
                    :thumbnail update-thumbnail
                    :body update-body
                    :media update-media))))

(defun append-update (author address chapter
                      &key (alias :null) (title :null) (thumbnail :null) (body :null) media)
  (with-transaction (trans)
    (%make-update trans author address chapter
                  :alias alias
                  :title title
                  :thumbnail thumbnail
                  :body body
                  :media media)))
