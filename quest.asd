(asdf:defsystem quest
  :serial t
  :depends-on (#:hunchentoot #:html-template #:parenscript #:cl-postgres+local-time #:postmodern #:alexandria #:anaphora #:routes #:ironclad #:zpng #:cffi #:cl-base64 #:cxml #:closer-mop)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             (:file "routes")
             (:file "templates")
             (:file "security")
             (:file "users")
             (:file "media")
             (:file "content")
             (:file "sessions")
             (:file "post-utils")
             (:file "random-art")
             (:file "setup")
             (:module "pages"
                      :serial t
                      :components
                      ((:file "icons")
                       (:file "login")
                       (:file "register")
                       (:file "index")
                       (:file "quest")
                       (:file "art")))))))
