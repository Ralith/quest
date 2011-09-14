(asdf:defsystem quest
  :serial t
  :depends-on (#:hunchentoot #:html-template #:parenscript #:cl-postgres+local-time #:postmodern #:alexandria #:anaphora #:routes #:ironclad #:zpng #:cffi #:cl-base64 #:cxml #:closer-mop)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "pomo-exts")
             (:file "utils")
             (:file "routes")
             (:file "templates")
             (:file "security")
             (:file "users")
             (:file "content")
             (:file "media")
             (:file "sessions")
             (:file "random-art")
             (:file "setup")
             (:module "pages"
                      :serial t
                      :components
                      ((:file "login")
                       (:file "register")
                       (:file "index")
                       (:file "quest")
                       (:file "art")))))))
