(in-package #:quest)

(defprepared-with-names create-post (address body &key (author-id :null) (title :null) (pen-name :null))
    ((:insert-into :posts :set :author :$1 :address :$2 :title :$3 :pen_name :$4 :body :$5
                   :returning :id)
     author-id address title pen-name body)
    :single)

(defprepared-with-names append-update (post-id chapter-id)
    ("INSERT INTO updates (post, chapter, ordinal) VALUES ($1, $2, get_update_ordinal($2)) RETURNING post"
     post-id chapter-id)
    :single)

(defprepared-with-names append-suggestion (post-id update-id)
    ("INSERT INTO suggestions (post, update, ordinal) VALUES ($1, $2, get_suggestion_ordinal($2)) RETURNING post"
     post-id update-id)
    :single)

(defprepared-with-names updates-of (chapter-id)
    ((:order-by (:select :post :from :updates :where (:= :chapter :$1))
                :ordinal)
     chapter-id)
    :column)

(defprepared-with-names post-details (post-id)
    ((:select :* :from :posts :where (:= :id :$1))
     post-id)
    :row)
