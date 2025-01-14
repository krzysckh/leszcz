(defpackage :leszcz
  (:use :common-lisp :nodgui)
  (:export
   :main))

(in-package :leszcz)

(defun main (argv)
  (declare (ignore argv))

  (nodgui:with-nodgui ()
    (nodgui:resizable *tk* 0 0)
    (let ((l (make-instance 'label :text "hello, world!")))
      (grid l 0 0 :padx 5 :pady 5)))
  )
