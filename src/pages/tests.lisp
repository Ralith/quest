(in-package :quest)

(defroute bbtest "/bbtest"
  (ecase (request-method*)
    (:post
     (with-params (:post bbcode)
       (template:fill-and-print-template (find-template "bbtest")
                                         (list :parsed (with-input-from-string (s bbcode)
                                                         (write-to-string (parse-bbcode s))))
                                         :stream (client-stream "application/xhtml+xml"))))
    (:get (template:fill-and-print-template (find-template "bbtest")
                                            (list :parsed nil)
                                            :stream (client-stream "application/xhtml+xml")))))