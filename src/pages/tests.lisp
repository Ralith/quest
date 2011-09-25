(in-package :quest)

(defroute bbtest "/bbtest"
  (ecase (request-method*)
    (:post
     (with-params (:post bbcode)
       (format t "Input: ~A" bbcode)
       (template:fill-and-print-template #p"bbtest.tmpl"
                                         (let ((tree (with-input-from-string (in bbcode)
                                                       (parse-bbcode in))))
                                           (list :parsed (escape-for-html (write-to-string tree))
                                                 :rendered (with-output-to-string (out)
                                                             (bbcode->html out tree))))
                                         :stream (client-stream "text/html"))))
    (:get (template:fill-and-print-template #p"bbtest.tmpl"
                                            (list :parsed nil)
                                            :stream (client-stream "text/html")))))
