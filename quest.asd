(asdf:defsystem quest
  :serial t
  :depends-on (#:hunchentoot #:html-template #:parenscript #:postmodern #:alexandria #:anaphora #:routes #:monkeylib-bcrypt)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "routes")
             (:file "templates")
             (:file "users")
             (:file "posts")
             (:file "quests")
             (:module "pages"
                      :serial t
                      :components
                      ((:file "index")
                       (:file "quest")))))))
