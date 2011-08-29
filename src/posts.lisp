(in-package #:quest)

(defclass post ()
  ((author :initarg :author
           :initform (error "Posts must have an author!")
           :reader author)
   (pen-name :initarg :pen-name
             :accessor pen-name
             :type string)
   (title :initarg :title
          :accessor title
          :type string)
   (body :initarg :body
         :accessor body
         :type string)))

(defprepared-with-names append-post (chapter author address body &key
                                      (suggestion t) (title :null) (pen-name :null))
    ("INSERT INTO posts (ordinal, chapter, author, address, suggestion, title, pen_name, body)
                  VALUES (get_post_ordinal($1), $1, $2, $3, $4, $5, $6, $7) RETURNING id"
     chapter author address suggestion title pen-name body)
    :single)
