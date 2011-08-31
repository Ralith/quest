(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: seed
(defroute art "/art"
  (setf (content-type*) "image/png")
  (random-art:render-to-stream (random-art:generate 4) (send-headers)))