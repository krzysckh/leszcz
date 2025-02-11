(defsystem "net"
  :depends-on (:leszcz-types :leszcz-constants :fast :alexandria :cl-ppcre :usocket)
  :components ((:file "net")))
