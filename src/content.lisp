(in-package #:quest)

(declaim (optimize (debug 3)))

(defdao content ()
    ((id :col-type serial :reader id)
     (type :col-type :content_type :initarg :type :reader content-type)
     (parent-id :col-type (or integer db-null) :initarg :parent-id :reader parent-id)
     (ordinal :col-type (or integer db-null) :initarg :ordinal :reader ordinal
              :documentation "Position of the content amongst its siblings")
     (child-count :col-type integer :col-default 0 :reader child-count
                  :documentation "Number of logical children had by the content (ignoring 1:1 relationships)")
     (user-id :col-type (or integer db-null) :initarg :user-id :reader user-id)
     (address :col-type inet :initarg :address :reader address)
     (title :col-type (or text db-null) :initarg :title :accessor title)
     (alias :col-type (or text db-null) :initarg :alias :accessor alias)
     (body :col-type (or text db-null) :initarg :body :accessor body)
     (thumbnail :col-type (or text db-null) :initarg :thumbnail :accessor thumbnail
                :documentation "Name of media to be displayed with item in indexes")
     (edited :col-type (or :timestamp-with-time-zone db-null) :accessor edited)
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created))
  (:keys id)
  (:foreign-key content parent-id id)
  (:foreign-key media (id thumbnail) (content-id name) :deferrable t :initially-deferred t)
  (:unique (parent-id ordinal)))
;;; TODO: Make thumbnail a deferrable foreign key reference
(closer-mop:finalize-inheritance (find-class 'content))

(defprepared-with-names alloc-ordinal (parent)
    ((:update 'content :set 'child-count (:+ 1 'child-count) :where (:= 'id :$1)
                                                             :returning 'child-count)
     (id parent))
    :single)

(defprepared-with-names %find-child (parent ordinal)
    ((:select :* :from 'content :where (:and (:= 'parent-id :$1)
                                             (:= 'ordinal :$2)))
     (id parent) ordinal)
    (:dao content :single))

(defun find-child (parent &rest ordinals)
  (if (first ordinals)
      (apply 'find-child (%find-child parent (first ordinals)) (rest ordinals))
      parent))

(macrolet ((def-dao-methods (type)
             (let ((type-name (string-downcase (symbol-name type))))
               `(progn
                  (defmethod get-dao ((type (eql ',type)) &rest args)
                    (aprog1 (apply 'get-dao 'content args)
                      (unless (string= (content-type it) ,type-name)
                        (error ,(concatenate 'string "~A is not of type " type-name) it))))
                  (defmethod make-dao ((type (eql ',type)) &rest args)
                    (apply 'make-dao 'content :type ,type-name args))))))
  (def-dao-methods board)
  (def-dao-methods quest)
  (def-dao-methods chapter)
  (def-dao-methods discussion)
  (def-dao-methods update)
  (def-dao-methods suggestion)
  (def-dao-methods post))

(defmethod print-object ((o content) s)
  (print-unreadable-object (o s)
    (format s "~A ~@[~A ~]{~A}"
            (string-upcase (content-type o))
            (subst nil :null (title o))
            (id o))))

(defun last-modified (content &aux (edited (edited content)))
  (if (eq edited :null)
      (created content)
      edited))

(defprepared-with-names updates (chapter)
    ((:order-by (:select :* :from 'content :where (:and (:= 'type "update")
                                                        (:= 'parent-id :$1)))
                'created)
     (id chapter))
    (:dao content))

(let ((query (concatenate 'string "
WITH RECURSIVE content_subtree AS (
    -- Base case
    SELECT $2::integer AS \"content_subtree_depth\", * FROM content WHERE parent_id = $1

    UNION ALL

    -- recursive term
    SELECT content_subtree.content_subtree_depth - 1 AS \"content_subtree_depth\", content.*
    FROM content
    JOIN content_subtree
    ON (content.parent_id = content_subtree.id)
    WHERE content_subtree.content_subtree_depth > 0
)
SELECT " (format nil "~{~A~^, ~}" (mapcar #'car (pomo::dao-column-map (find-class 'content)))) "
FROM content_subtree
ORDER BY created")))
  (defprepared-with-names content-subtree-desc (content depth)
     ((concatenate 'string query " DESC")
      (id content) depth)
     (:dao content))
  (defprepared-with-names content-subtree-asc (content depth)
     ((concatenate 'string query " ASC")
      (id content) depth)
     (:dao content)))

(defprepared-with-names content-parents (content)
    ("
WITH RECURSIVE content_parents AS (
    SELECT * FROM content WHERE id = $1

    UNION ALL

    SELECT content.*
    FROM content
    JOIN content_parents
    ON (content.id = content_parents.parent_id)
) SELECT * FROM content_parents
"
     (id content))
    (:dao content))

(defprepared-with-names chapters (quest)
    ((:order-by (:select :* :from 'content :where (:and (:= 'type "chapter")
                                                        (:= 'parent-id :$1)))
                'created)
     (id quest))
    (:dao content))

(defprepared-with-names latest-chapter (quest)
    ((:limit (:order-by (:select :* :from 'content :where (:and (:= 'type "chapter")
                                                                (:= 'parent-id :$1)))
                        (:desc 'created))
             1)
     (id quest))
    (:dao content :single))
