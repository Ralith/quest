(in-package #:quest)

(declaim (optimize (debug 3)))

;;; TODO: seed
(defun make-seed ()
  (random (expt 2 48)))


(defroute art-seed-redirect "/random-art"
  (redirect (with-output-to-string (s)
              (princ "/art/" s)
              (write (make-seed) :base 36 :stream s))))

(defroute random-art-sized "/random-art/:(x)x:(y)"
  (redirect
   (with-output-to-string (s)
     (princ "/art/" s)
     (write (make-seed) :base 36 :stream s)
     (format s "/~Dx~D" (parse-integer x) (parse-integer y)))))

(defroute art-index "/art/"
  (with-output-to-string (s)
    (fill-and-print-template (find-template "art-index")
                             (list :small "128x128"
                                   :large "512x512"
                                   :seeds
                                   (loop repeat 10 collect (list :seed (write-to-string (make-seed) :base 36))))
                             :stream s)))



(defroute art-size-redirect "/art/:seed"
  (redirect (format nil "/art/~A/size/~Dx~D" seed 512 512)))

(defroute art "/art/:seed/size/:(x)x:(y)"
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
