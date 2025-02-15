(defsystem "leszcz"
  :depends-on (:leszcz-types :leszcz-constants :raylib :gui :alexandria :cl-ppcre :cl-mop :net)
  :components ((:file "leszcz") (:file "bot"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "Main entry and logic of leszcz."
  )
