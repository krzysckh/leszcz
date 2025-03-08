(defsystem "leszcz"
  :depends-on (:alexandria
               :cl-ppcre
               :cl-store
               :local-time
               :bordeaux-threads
               :file-select
               :leszcz-types
               :leszcz-constants
               :raylib
               :gui
               :net)
  :components ((:file "leszcz") (:file "bot") (:file "book"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "Main entry and logic of leszcz."
  )
