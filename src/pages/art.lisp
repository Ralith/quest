(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: seed
(defroute art "/art"
  (setf (content-type*) "image/png")
  (random-art:render-to-stream (random-art:generate 5) (send-headers)))

(defroute sized-art "/art/:(x)x:(y)"
  (setf (content-type*) "image/png")
  (random-art:render-to-stream (random-art:generate 5) (send-headers)
                               (parse-integer x) (parse-integer y)))
