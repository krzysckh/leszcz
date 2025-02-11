(defsystem "fast"
  :depends-on (:leszcz-types :leszcz-constants :alexandria :cl-ppcre)
  :components ((:file "fast"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "Faster board operations for leszcz."
  )
