(cl:in-package #:asdf-user)

(defsystem "salvager-gui"
  :serial t
  :components
  ((:file "packages")
   (:file "gui")))
