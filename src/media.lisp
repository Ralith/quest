(in-package #:quest)

;;; UI note: Any media may be thumbnail; UI should default to setting the first upload.
(defdao media ()
    ((content-id :col-type integer :initarg :content-id :reader content-id)
     (name :col-type text :initarg :name :accessor name)
     (checksum :col-type bytea :initarg :checksum :accessor checksum))
  (:foreign-key content content-id id)
  (:unique content-id name))
