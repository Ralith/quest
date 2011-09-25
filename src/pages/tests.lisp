(in-package :quest)

(defroute bbtest "/bbtest"
  (ecase (request-method*)
    (:post
     (with-params (:post bbcode)
       (template:fill-and-print-template #p"bbtest.tmpl"
                                         (let ((tree (with-input-from-string (in bbcode)
                                                       (parse-bbcode in))))
                                           (list :input (escape-for-html bbcode)
                                                 :parsed (escape-for-html (write-to-string tree))
                                                 :rendered (with-output-to-string (out)
                                                             (bbcode->html out tree))))
                                         :stream (client-stream "text/html"))))
    (:get (template:fill-and-print-template #p"bbtest.tmpl"
                                            (list :input ""
                                                  :parsed nil
                                                  :rendered nil)
                                            :stream (client-stream "text/html")))))
