(asdf:defsystem quest
  :serial t
  :depends-on (#:hunchentoot #:html-template #:parenscript #:postmodern #:alexandria #:anaphora #:routes #:ironclad #:zpng #:cffi #:cl-base64)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "pomo-exts")
             (:file "utils")
             (:file "routes")
             (:file "templates")
             (:file "users")
             (:file "content")
             (:file "sessions")
             (:file "random-art")
             (:module "pages"
                      :serial t
                      :components
                      ((:file "index")
                       (:file "quest")
                       (:file "art")))))))
