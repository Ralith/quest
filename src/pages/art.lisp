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
  (let ((seed (parse-integer seed :radix 36)))
    (setf seed
          (apply #'logxor (loop for i from 0
                                for chunk = (ldb (byte 48 (* 48 i)) seed)
                                until (= chunk 0)
                                collect chunk)))
    (random-art:render-to-stream (random-art:generate :seed seed)
                                 (send-headers)
                                 (parse-integer x) (parse-integer y))))
