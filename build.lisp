;; fallback if ql not loaded by userinit
(load (truename #P"~/quicklisp/setup.lisp"))
(require 'uiop)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(push (pathname (uiop:getcwd)) ql:*local-project-directories*)

(ql:quickload :leszcz ) ; :verbose t)

(defparameter *bin-path*
  (if (uiop/os:os-windows-p)
      "build/leszcz.exe"
      "build/leszcz"))

(defparameter *base-build-options*
  `(:executable t
    :toplevel ,#'leszcz:main
    ))

(defparameter *build-options*
  (append
   *base-build-options*
   (if (uiop/os:os-windows-p)
       '(:application-type :gui
         )
       '(
         ))))

(setf leszcz-constants:*prod* t)
(setf leszcz-constants:*debug* nil)

(apply #'sb-ext:save-lisp-and-die (append (list *bin-path*) *build-options*))
