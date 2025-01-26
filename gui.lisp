(defpackage :gui
  (:use :common-lisp :raylib :leszcz-types :leszcz-constants :cl-ppcre)
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

(defparameter +color-grayish+ '(48 48 48 200))
(defparameter +color-hax0r-green+ '(0 255 0 255))

(defparameter toplevel-console/state "")
(defparameter toplevel-console/log nil)
(defparameter toplevel-console/console-on-screen-p nil)
(defparameter toplevel-console/font-size 18)
(defparameter toplevel-console/height (/ *window-height* 2))
(defun toplevel-console (g)
  (declare (ignore g))

  (setf toplevel-console/console-on-screen-p t)

  (draw-rectangle 0 0 *window-width* toplevel-console/height +color-grayish+)

  (setf toplevel-console/state
        (format nil "~a~a" toplevel-console/state (coerce (get-chars-pressed) 'string)))

  (when (key-pressed-p-1 259)
    (when (> (length toplevel-console/state) 0)
      (setf toplevel-console/state
            (subseq toplevel-console/state 0 (- (length toplevel-console/state) 1)))))

  (when (key-pressed-p-1 257)
    (setf toplevel-console/log
          (append
           toplevel-console/log
           (list
            (format nil ";; ~a" toplevel-console/state))
           (let ((thing
                   (handler-case (eval (read-from-string toplevel-console/state))
                     (error (c)
                       (cl-ppcre:split "\\n" (format nil "; Error: ~a" c))))))
             (if (listp thing) thing (list thing)))))
    (setf toplevel-console/state ""))

  (let* ((delta (- toplevel-console/height (* (1+ (length toplevel-console/log)) toplevel-console/font-size))))
    (when (< delta 0)
      (setf toplevel-console/log
            (nthcdr (ceiling (/ (- delta) toplevel-console/font-size)) toplevel-console/log))))

  (loop for i from 0 to (- (length toplevel-console/log) 1) do
    (draw-text
     (format nil "~a" (nth i toplevel-console/log))
     0
     (* toplevel-console/font-size i)
     toplevel-console/font-size
     +color-hax0r-green+))

  (draw-text
   (format nil "toplevel-console> ~a" toplevel-console/state)
   0 (* (length toplevel-console/log) toplevel-console/font-size)
   toplevel-console/font-size +color-hax0r-green+)
  )

(defun toplevel-console-listener (&rest r)
  (when (key-pressed-p #\`)
    (if toplevel-console/console-on-screen-p
        (progn
          (remove-draw-hook 'toplevel-console)
          (setf toplevel-console/console-on-screen-p nil))
        (add-draw-hook 'toplevel-console))))
