(in-package #:quest)

(declaim (optimize (debug 3)))

(defdao content ()
    ((id :col-type serial :reader id)
     (type :col-type :content_type :initarg :type :reader content-type)
     (parent-id :col-type (or integer db-null) :initarg :parent-id :reader parent-id)
     (user-id :col-type (or integer db-null) :initarg :user-id :reader user-id)
     (address :col-type inet :initarg :address :reader address)
     (title :col-type (or text db-null) :initarg :title :accessor title)
     (alias :col-type (or text db-null) :initarg :alias :accessor alias)
     (body :col-type (or text db-null) :initarg :body :accessor body)
     (edited :col-type (or :timestamp-with-time-zone db-null) :accessor edited)
     (created :col-type :timestamp-with-time-zone :col-default (:now) :reader created))
  (:keys id)
  (:foreign-key content parent-id id))

(macrolet ((def-get-dao (type)
             (let ((type-name (string-downcase (symbol-name type))))
               `(defmethod get-dao ((type (eql ',type)) &rest args)
                  (aprog1 (apply #'get-dao 'content args)
                    (unless (string= (content-type it) ,type-name)
                      (error ,(concatenate 'string "~A is not of type " type-name) it)))))))
  (def-get-dao board)
  (def-get-dao quest)
  (def-get-dao chapter)
  (def-get-dao discussion)
  (def-get-dao update)
  (def-get-dao suggestion)
  (def-get-dao post))

(defmethod print-object ((o content) s)
  (print-unreadable-object (o s )
    (format s "~A ~@[~A ~]{~A}"
            (string-upcase (content-type o))
            (subst nil :null (title o))
            (id o))))

(defprepared-with-names updates (chapter)
    ((:order-by (:select :* :from 'content :where (:and (:= 'type "update")
                                                        (:= 'parent-id :$1)))
                'created)
     (id chapter))
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
