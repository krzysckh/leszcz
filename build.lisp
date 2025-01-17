;; fallback if ql not loaded by userinit
(load (truename #P"~/quicklisp/setup.lisp"))
(require 'uiop)

(push (pathname (uiop:getcwd)) ql:*local-project-directories*)

(ql:quickload :leszcz)

(defparameter *bin-path*
  (if (uiop/os:os-windows-p)
      "build/leszcz.exe"
      "build/leszcz"))

(sb-ext:save-lisp-and-die
 *bin-path*
 :executable t
 :toplevel #'leszcz:main)
