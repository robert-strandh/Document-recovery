(cl:in-package #:asdf-user)

(defsystem "salvager-slope"
  :serial t
  :components
  ((:file "packages")
   (:file "determine-slope")))
