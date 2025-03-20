;; fallback if ql not loaded by userinit
(load (truename #P"~/quicklisp/setup.lisp"))

(declaim (optimize (speed 3) (space 3) (debug 0) (safety 0)))

(push (pathname (uiop:getcwd)) ql:*local-project-directories*)

(ql:quickload :leszcz ) ; :verbose t)

(defun windbgp ()
  (boundp 'COMMON-LISP-USER::WINDBG))

(when (windbgp)
  (format t "compiling with WINDBG flag~%"))

(defparameter *bin-path*
  (cond
    ((and (uiop/os:os-windows-p) (windbgp))
     "build/leszcz-debug.exe")
    ((uiop/os:os-windows-p)
     "build/leszcz.exe")
    (t
     "build/leszcz")))

(defparameter *base-build-options*
  `(:executable t
    :toplevel ,#'leszcz:main
    ))

(defparameter *build-options*
  (append
   *base-build-options*
   (if (and (uiop/os:os-windows-p) (not (windbgp)))
       '(:application-type :gui
         )
       '(
         ))))

(setf leszcz-constants:*prod* t)
(setf leszcz-constants:*debug* nil)

(apply #'sb-ext:save-lisp-and-die (append (list *bin-path*) *build-options*))
