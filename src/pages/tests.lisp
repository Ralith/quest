(in-package :quest)

(defroute bbtest "/bbtest"
  (template:fill-and-print-template
   #p"bbtest.tmpl"
   (ecase (request-method*)
     (:post (with-params (:post bbcode)
              (let ((tree (with-input-from-string (in bbcode)
                            (parse-bbcode in))))
                (list :input (escape-for-html bbcode)
                      :parsed (escape-for-html (write-to-string tree))
                      :rendered (with-output-to-string (out)
                                  (bbcode->html out tree))))))
     (:get (list :input "" :parsed nil :rendered nil)))
   :stream (client-stream "text/html")))
