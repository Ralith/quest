(in-package :quest)

(defroute bbtest "/bbtest"
  (let ((template:*string-modifier* #'identity))
   (ecase (request-method*)
     (:post
      (with-params (:post bbcode)
        (template:fill-and-print-template (find-template "bbtest")
                                          (let ((tree (with-input-from-string (in bbcode)
                                                        (parse-bbcode in))))
                                            (list :parsed (escape-for-html (write-to-string tree))
                                                  :rendered (with-output-to-string (out)
                                                              (bbcode->html out tree))))
                                          :stream (client-stream "application/xhtml+xml"))))
     (:get (template:fill-and-print-template (find-template "bbtest")
                                             (list :parsed nil)
                                             :stream (client-stream "application/xhtml+xml"))))))
