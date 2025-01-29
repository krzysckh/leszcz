(defpackage :gui
  (:use :alexandria :common-lisp :raylib :leszcz-types :leszcz-constants :cl-ppcre)
  (:export
   capturer
   keys-can-be-captured-p
   current-capturer-can-be-removed-p
   current-capturer
   set-current-capturer!
   delete-current-capturer! ;; This can be used only by the capturer owner itself

   mainloop-draw-hooks

   add-draw-hook
   remove-draw-hook

   toplevel-console-listener

   text-button
   make-button
   ))

(in-package :gui)

(defclass capturer ()
  ((can-be-removed-p
    :initarg :can-be-removed-p
    :accessor capturer-can-be-removed-p
    :initform t)
   (delete
    :initarg :delete
    :accessor capturer-delete
    :initform #'(lambda () t))))

(defparameter current-capturer nil)

(defun current-capturer-can-be-removed-p ()
  (if-let ((c current-capturer))
    (capturer-can-be-removed-p c)
    t))

(defun keys-can-be-captured-p (self)
  (declare (type capturer self))
  (equal self current-capturer))

(defun set-current-capturer! (c)
  (if (keys-can-be-captured-p c)
      t ;; c is already the current capturer
      (if (current-capturer-can-be-removed-p)
          (progn
            (when current-capturer
              (funcall (capturer-delete current-capturer)))
            (setf current-capturer c))
          nil)))

(defun delete-current-capturer! ()
  (setf current-capturer nil))

(defparameter mainloop-draw-hooks nil)

(defun add-draw-hook (fn)
  (push fn mainloop-draw-hooks))

(defun remove-draw-hook (name)
  (setf mainloop-draw-hooks (remove name mainloop-draw-hooks)))

(defparameter +color-grayish+ '(48 48 48 200))
(defparameter +color-hax0r-green+ '(0 255 0 255))

(defparameter toplevel-console/capturer (make-instance 'capturer :can-be-removed-p nil))
(defparameter toplevel-console/state "")
(defparameter toplevel-console/log nil)
(defparameter toplevel-console/font-size 18)
(defparameter toplevel-console/height (/ *window-height* 2))

(defun toplevel-console (g)
  (declare (ignore g))

  ;; the listener should check for capturer rights, but let's check and report bugs if something is wrong
  (when (not (keys-can-be-captured-p toplevel-console/capturer))
    (warn "toplevel-console caught BUG: getting called without capturer rights"))

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
   (format nil "> ~a" toplevel-console/state)
   0 (* (length toplevel-console/log) toplevel-console/font-size)
   toplevel-console/font-size +color-hax0r-green+)
  )

(defun toplevel-console-listener (&rest r)
  (when (key-pressed-p #\`)
    (if (keys-can-be-captured-p toplevel-console/capturer)
        (progn ;; we're on screen -- exit
          (remove-draw-hook 'toplevel-console)
          (delete-current-capturer!))
        (progn ;; we're not on screen, try getting capturer rights and add hooks
          (when (set-current-capturer! toplevel-console/capturer)
            (add-draw-hook 'toplevel-console))))))

(defparameter +color-light-brownish+   '(#x54 #x33 #x09 #xff))
(defparameter +color-lighter-brownish+ '(#x74 #x53 #x10 #xff))
(defparameter +color-dark-brownish+    '(#x2f #x17 #x08 #xff))
(defparameter +color-golden+           '(#xf2 #xd3 #x0e #xff))

(defun text-button (x y w h text &key (font-size (- h 2)) (pad t))
  (let ((at-point-p (point-in-rect-p (floatize (list (mouse-x)
                                                     (mouse-y)))
                                     (floatize (list x y w h)))))
    (if at-point-p
        (set-mouse-cursor! +cursor-pointer+)
        (set-mouse-cursor! +cursor-normal+))
    (draw-rectangle x y w h (if at-point-p
                                +color-lighter-brownish+
                                +color-light-brownish+))
    (when pad
      (draw-rectangle-lines-2
       (floatize (list (- x 8) (- y 8) (+ w 16) (+ h 16)))
       (float 8)
       +color-dark-brownish+))
    (draw-text text x y font-size +color-golden+)
    (and at-point-p (mouse-pressed-p 0))))

(defun make-button (text font-size)
  (let* ((font (load-font spleen-data font-size))
         (v2 (measure-text font text (float font-size) 0.0)))
    (values
     #'(lambda (x y)
         (text-button
          x y (floor (car v2)) (floor (cadr v2))
          text :font-size font-size))
     (car v2)
     (cadr v2))))
