(in-package #:quest)

(declaim (optimize (debug 3)))

(defprepared-with-names %append-chapter (quest &optional (title :null))
    ("INSERT INTO chapters (title, quest, ordinal)
                  VALUES ($1, $2, get_chapter_ordinal($2)) RETURNING id"
     title quest)
    :single)

(defprepared-with-names posts-of (chapter)
    ((:order-by (:select :* :from '#:posts :where (:= '#:chapter :$1))
                (:desc '#:ordinal))
     chapter))

(defprepared-with-names %create-quest (author &optional (title :null))
    ((:insert-into '#:quests :set '#:author :$1 '#:title :$2 :returning '#:id)
     author title)
    :single)

(defprepared-with-names author-of (quest)
    ((:select '#:author :from '#:quests :where (:= '#:id :$1))
     quest)
    :single)

(defprepared-with-names chapters (quest)
    ((:order-by (:select :* :from '#:chapters :where (:= '#:quest :$1))
                '#:ordinal)
     quest))

(defprepared-with-names latest-chapter (quest)
    ((:limit (:order-by (:select :* :from '#:chapters :where (:= '#:quest :$1))
                        (:desc '#:ordinal))
             1)
     quest)
    :row)

(defprepared-with-names id->quest (id)
    ((:limit (:select :* :from '#:quests :where (:= '#:id :$1))
             1)
     id)
    :row)

(defun append-chapter (quest address post-body &key title pen-name)
  (with-transaction ()
    (aprog1 (%append-chapter quest (or title :null))
      (append-post it (author-of quest) address post-body :suggestion nil :pen-name pen-name))))

(defun create-quest (author address post-body &key title chapter-title pen-name)
  (with-transaction ()
    (aprog1 (%create-quest author (or title :null))
      (append-post (%append-chapter it (or chapter-title :null))
                   author address post-body :suggestion nil :pen-name pen-name))))