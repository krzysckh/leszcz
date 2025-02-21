(defsystem "leszcz"
  :depends-on (:alexandria
               :cl-ppcre
               :local-time
               :bordeaux-threads
               :leszcz-types
               :leszcz-constants
               :raylib
               :gui
               :net)
  :components ((:file "leszcz") (:file "bot"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "Main entry and logic of leszcz."
  )
