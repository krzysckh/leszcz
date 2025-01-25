(defpackage :gui
  (:use :common-lisp :raylib :leszcz-types :leszcz-constants)
  (:export
   mainloop-draw-hooks

   add-draw-hook
   remove-draw-hook

   toplevel-console-listener
   ))

(in-package :gui)

(defparameter mainloop-draw-hooks nil)

(defun add-draw-hook (fn)
  (push fn mainloop-draw-hooks))

(defun remove-draw-hook (name)
  (setf mainloop-draw-hooks (remove name mainloop-draw-hooks)))

(defparameter +color-grayish+ '(100 100 100 200))
(defparameter +color-hax0r-green+ '(0 255 0 255))

(defparameter toplevel-console/state "")
(defparameter toplevel-console/console-on-screen-p nil)
(defun toplevel-console (g delete)
  (declare (type leszcz-types:game g))

  (setf toplevel-console/console-on-screen-p t)

  (draw-rectangle 0 0 *window-width* (/ *window-height* 2) +color-grayish+)

  (setf toplevel-console/state
        (format nil "~a~a" toplevel-console/state (coerce (get-chars-pressed) 'string)))

  (draw-text
   (format nil "toplevel-console> ~a" toplevel-console/state)
   0 0 16 +color-hax0r-green+)

  (when (key-pressed-p #\`)
    (funcall delete))
  )

(defun toplevel-console-listener (&rest r)
  (when (key-pressed-p #\`)
    (if toplevel-console/console-on-screen-p
        (remove-draw-hook 'toplevel-console)
        (add-draw-hook 'toplevel-console))))
