(in-package :quest)

(defroute bbtest "/bbtest"
  (template:fill-and-print-template
   #p"bbtest.tmpl"
   (ecase (request-method*)
     (:post (with-params (:post bbcode)
              (handler-case
                  (let ((tree (with-input-from-string (in bbcode)
                                (parse-bbcode in))))
                    (list :error nil
                          :input (escape-for-html bbcode)
                          :parsed (escape-for-html (write-to-string tree))
                          :rendered (with-output-to-string (out)
                                      (bbcode->html out tree))))
                (bbcode-error (e) (list :error (escape-for-html (princ-to-string e))
                                        :input (escape-for-html bbcode)
                                        :parsed nil
                                        :rendered nil)))))
     (:get (list :error nil :input "" :parsed nil :rendered nil)))
   :stream (client-stream "text/html")))
