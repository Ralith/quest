(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: seed
(defun make-seed ()
  (random (expt 2 48)))

(defroute art-seed-redirect "/art"
  (redirect (with-output-to-string (s)
              (princ "/art/" s)
              (write (make-seed) :base 36 :stream s))))

(defroute art-size-redirect "/art/:seed"
  (redirect (format nil "/art/~A/~Dx~D" seed 512 512)))

(defroute art "/art/:seed/:(x)x:(y)"
  (setf (content-type*) "image/png")
  (random-art:render-to-stream (random-art:generate :seed (parse-integer seed :radix 36))
                               (send-headers)
                               (parse-integer x) (parse-integer y)))
