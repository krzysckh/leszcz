(defsystem "gui"
  :depends-on (:raylib :leszcz-types :leszcz-constants :cl-ppcre)
  :components ((:file "gui"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "some GUI functions for leszcz."
  )
