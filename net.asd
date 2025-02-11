(defsystem "net"
  :depends-on (:leszcz-types :leszcz-constants :fast :alexandria :cl-ppcre :usocket)
  :components ((:file "net"))
  :license "BSD 2-Clause"
  :author "Krzysztof Michałczyk <kpm@krzysckh.org>"
  :description "Network protocol implementation for leszcz."
  )
