(in-package #:quest)

(declaim (optimize (debug 3)))

(defprepared-with-names %append-chapter (quest-id &optional (title :null))
    ("INSERT INTO chapters (title, quest, ordinal)
                  VALUES ($1, $2, get_chapter_ordinal($2)) RETURNING id"
     title quest-id)
    :single)

(defprepared-with-names %create-quest (author-id &optional (title :null))
    ((:insert-into '#:quests :set '#:author :$1 '#:title :$2 :returning '#:id)
     author-id title)
    :single)

(defprepared-with-names quest-author-id (quest-id)
    ((:select '#:author :from '#:quests :where (:= '#:id :$1))
     quest-id)
    :single)

(defprepared-with-names chapter-ids (quest-id)
    ((:order-by (:select :id :from '#:chapters :where (:= '#:quest :$1))
                '#:ordinal)
     quest-id)
    :column)

(defprepared-with-names latest-chapter-id (quest-id)
    ((:limit (:order-by (:select :id :from :chapters :where (:= :quest :$1))
                        (:desc :ordinal))
             1)
     quest-id)
    :single)

(defprepared-with-names quest-details (id)
    ((:limit (:select :* :from '#:quests :where (:= '#:id :$1))
             1)
     id)
    :row)

(defprepared-with-names user-details (id)
    ((:limit (:select :* :from '#:users :where (:= '#:id :$1))
             1)
     id)
    :row)

(defprepared-with-names chapter-details (id)
    ((:limit (:select :* :from '#:chapters :where (:= '#:id :$1))
             1)
     id)
    :row)

(defun append-chapter (quest-id address post-body &key title pen-name)
  (with-transaction ()
    (aprog1 (%append-chapter quest-id (or title :null))
      (append-update (create-post address post-body
                                  :author-id (quest-author-id quest-id)
                                  :pen-name (or pen-name :null))
                     it))))

(defun create-quest (author-id address post-body &key title chapter-title pen-name)
  (with-transaction ()
    (aprog1 (%create-quest author-id (or title :null))
      (append-update (create-post address post-body
                                  :author-id author-id
                                  :pen-name (or pen-name :null))
                     (%append-chapter it (or chapter-title :null))))))