(in-package :quest)

(defroute bbtest "/bbtest"
  (ecase (request-method*)
    (:post
     (with-params (:post bbcode)
       (let ((template:*string-modifier* #'identity))
        (template:fill-and-print-template (find-template "bbtest")
                                          (list :parsed (with-output-to-string (out)
                                                          (with-input-from-string (in bbcode)
                                                            (bbcode->html out (parse-bbcode in)))))
                                          :stream (client-stream "application/xhtml+xml")))))
    (:get (template:fill-and-print-template (find-template "bbtest")
                                            (list :parsed nil)
                                            :stream (client-stream "application/xhtml+xml")))))
