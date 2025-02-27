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
   texture-button
   input-box
   make-button
   make-button*
   make-input-box

   switch-textures-to
   configure-menu
   input-box/content-ht
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
              (funcall (the function (capturer-delete current-capturer))))
            (setf current-capturer c))
          nil)))

(defun delete-current-capturer! ()
  (setf current-capturer nil))

(defparameter mainloop-draw-hooks nil)

(defun add-draw-hook (fn)
  (push fn mainloop-draw-hooks))

(defun remove-draw-hook (name)
  (setf mainloop-draw-hooks (remove name mainloop-draw-hooks)))

(defparameter +color-grayish-2+ '(20 20 20 200))
(defparameter +color-hax0r-green+ '(0 255 0 255))

(defparameter toplevel-console/capturer (make-instance 'capturer :can-be-removed-p nil))
(defparameter toplevel-console/state "")
(defparameter toplevel-console/log nil)
(defparameter toplevel-console/font-size 18)
(defparameter toplevel-console/height (/ *window-height* 2))

(declaim (type list toplevel-console/log))
(declaim (type integer toplevel-console/font-size toplevel-console/height))

(defun toplevel-console (g)
  (declare (ignore g)
           #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

  ;; the listener should check for capturer rights, but let's check and report bugs if something is wrong
  (when (not (keys-can-be-captured-p toplevel-console/capturer))
    (warn "toplevel-console caught BUG: getting called without capturer rights"))

  (draw-rectangle 0 0 *window-width* toplevel-console/height +color-grayish-2+)

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
            (nthcdr (ceiling (the integer (/ (- delta) toplevel-console/font-size))) toplevel-console/log))))

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
  (declare (ignore r))
  (when (key-pressed-p #\`)
    (if (keys-can-be-captured-p toplevel-console/capturer)
        (progn ;; we're on screen -- exit
          (remove-draw-hook 'toplevel-console)
          (delete-current-capturer!))
        (progn ;; we're not on screen, try getting capturer rights and add hooks
          (when (set-current-capturer! toplevel-console/capturer)
            (add-draw-hook 'toplevel-console))))))

(defparameter tb/color-bg                '(#x11 #x11 #x11 #xff))
(defparameter tb/color-bg-hover          '(#x22 #x22 #x22 #xff))
(defparameter tb/color-margin            '(#xe4 #xf6 #x14 #xff))
(defparameter tb/color-margin-unselected '(#xa4 #x51 #xee #xff))
(defparameter tb/color-text              '(#xde #xde #xde #xff))

(defparameter tb/padx 32)

(defparameter input-box/content-ht (make-hash-table))

(defun make-input-box (id &key height width (font-data spleen-data) (font-hash raylib::*font*) (text-draw-fn #'draw-text) (default-value nil))
  (when default-value
    (setf (gethash id input-box/content-ht) (coerce default-value 'list)))

  (values
   #'(lambda (x y &rest _)
       (declare (ignore _))
       (input-box id x y width height :text-draw-fn text-draw-fn))
   width
   height))

(defun input-box (id x* y* w* h* &key (font-size (round (- h* 2))) (text-draw-fn #'raylib:draw-text))
  (declare (type function text-draw-fn))
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (let-values ((x y w h (values (round x*) (round y*) (round w*) (round h*)))
               (full-rect (list (- x tb/padx) (- y 8) (+ w (* tb/padx 2)) (+ h 16)))
               (at-point-p (point-in-rect-p (floatize (list (mouse-x) (mouse-y))) (floatize full-rect))))
    (draw-rectangle (nth 0 full-rect)
                    (nth 1 full-rect)
                    (nth 2 full-rect)
                    (nth 3 full-rect)
                    (if at-point-p
                        tb/color-bg-hover
                        tb/color-bg))

    (draw-rectangle-lines-2
     (floatize (list (- x tb/padx) (- y 8) (+ w (* tb/padx 2)) (+ h 16)))
     (float 2)
     (if at-point-p
         tb/color-margin
         tb/color-margin-unselected))

    (when at-point-p
      (set-mouse-cursor! +cursor-pointer+)
      (when (key-pressed-p-1 259)
        (setf (gethash id input-box/content-ht) (butlast (gethash id input-box/content-ht))))
      (setf (gethash id input-box/content-ht) (append (gethash id input-box/content-ht)
                                                      (remove-if
                                                       #'(lambda (c) (> (char-int c) 255))
                                                       (get-chars-pressed-1)))))

    (funcall
     text-draw-fn
     (coerce (gethash id input-box/content-ht) 'string)
     (+ (nth 0 full-rect) 8)
     y
     font-size
     tb/color-text)))

(defun text-button (x* y* w* h* text text-width &key (font-size (round (- h* 2))) (text-draw-fn #'raylib:draw-text))
  (declare (type function text-draw-fn))
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (let-values ((x y w h (values (round x*) (round y*) (round w*) (round h*)))
               (full-rect (list (- x tb/padx) (- y 8) (+ w (* tb/padx 2)) (+ h 16)))
               (at-point-p (point-in-rect-p (floatize (list (mouse-x) (mouse-y))) (floatize full-rect))))
    (if at-point-p
        (set-mouse-cursor! +cursor-pointer+)
        (set-mouse-cursor! +cursor-normal+))

    (draw-rectangle (nth 0 full-rect)
                    (nth 1 full-rect)
                    (nth 2 full-rect)
                    (nth 3 full-rect)
                    (if at-point-p
                        tb/color-bg-hover
                        tb/color-bg))

    (draw-rectangle-lines-2
     (floatize (list (- x tb/padx) (- y 8) (+ w (* tb/padx 2)) (+ h 16)))
     (float 2)
     (if at-point-p
         tb/color-margin
         tb/color-margin-unselected))

    (funcall text-draw-fn text (+ (nth 0 full-rect) (/ (- (nth 2 full-rect) text-width) 2)) y font-size tb/color-text)
    (and at-point-p (mouse-pressed-p 0))))

(defun texture-button (x y w h texture &key (pad t) (background-color nil))
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((at-point-p (point-in-rect-p (floatize (list (mouse-x)
                                                     (mouse-y)))
                                     (floatize (list x y w h)))))
    (if at-point-p
        (set-mouse-cursor! +cursor-pointer+)
        (set-mouse-cursor! +cursor-normal+))
    (when background-color
      (draw-rectangle x y w h background-color))
    (draw-texture
     texture
     (floatize (list 0 0 (cadr texture) (caddr texture)))
     (floatize (list x y w h))
     (floatize (list 0 0))
     (float 0)
     (if at-point-p
         tb/color-margin
         +color-white+))
    (when pad
      (draw-rectangle-lines-2
       (floatize (list (- x 8) (- y 8) (+ w 16) (+ h 16)))
       (float 4)
       tb/color-margin))
    (and at-point-p (mouse-pressed-p 0))))

(defun make-button* (text-or-texture &key height width background-color identifier (font-data spleen-data) (font-hash raylib::*font*) (text-draw-fn #'draw-text))
  (multiple-value-bind (f a b)
      (make-button text-or-texture :height height :width width :background-color background-color :text-draw-fn text-draw-fn :font-hash font-hash :font-data font-data)
    (values
     #'(lambda (x y f*)
         (when (funcall (the function f) x y)
           (set-mouse-cursor! +cursor-normal+)
           (funcall (the function f*) identifier)))
     a
     b)))

(defun make-button (text-or-texture &key height width background-color (font-data spleen-data) (font-hash raylib::*font*) (text-draw-fn #'draw-text))
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

  (when (null height)
    (error "Make-button: height required"))
  (if (stringp text-or-texture)
      (let* ((font (load-font font-data height :loaded-font-hash font-hash))
             (v2 (measure-text font text-or-texture (float height) 0.0)))
        (values
         #'(lambda (x y)
             (text-button
              x y (floor (car v2)) (floor (cadr v2))
              text-or-texture
              (car v2)
              :font-size height
              :text-draw-fn text-draw-fn))
         (car v2)
         (cadr v2)))
      (progn
        (when (null width)
          (error "Make-button requires width when making a textured button"))
        (values
         #'(lambda (x y)
             (texture-button
              x y width height (if (functionp text-or-texture) (funcall (the function text-or-texture)) text-or-texture)
              :background-color background-color))
         width
         height))))

(defun unload-textures! (alist)
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop for c in alist do
    (if (vectorp (cdr c))
        (loop for x across (cdr c) do (unload-texture! x))
        (unload-texture! (cdr c)))))

(defun switch-textures-to (sym)
  (declare (type symbol sym))
  (unload-textures! white-texture-alist)
  (unload-textures! black-texture-alist)

  (when-let ((texture-pack (cdr (assoc sym texture-options))))
    (setf white-texture-data-list (cdr (assoc 'white texture-pack)))
    (setf black-texture-data-list (cdr (assoc 'black texture-pack)))
    (setf *color-bg-light* (cdr (assoc 'bg-light texture-pack)))
    (setf *color-bg-dark*  (cdr (assoc 'bg-dark texture-pack))))

  (format t "colors: ~a, ~a ~%" *color-bg-light* *color-bg-dark*)

  (load-textures))

;; Steal mainloop and show a menu that can configure data from :leszcz-constants (not so constant now huh?)
;; TODO: I don't actually think this should steal the mainloop, but it's easier for now as it will
;;       make it impossible to fuck up some clicks and clacks u know
;;       i will fix it later to use the cool ass capturer mechanism
(defun configure-menu ()
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note warning))
  (let-values ((b1 w1 h1 (make-button* "sleek" :height 24 :identifier 'sleek))
               (b2 w2 h2 (make-button* "pixel" :height 24 :identifier 'pixel))
               (font (load-font spleen-data 24))
               (ws (measure-text font "Dostępne paczki tekstur: " 24.0 0.0))
               (wt (car ws)))
    (loop until (or (window-close-p) (key-pressed-p-1 256)) do
      (begin-drawing)

      (clear-background +color-grayish+)

      (draw-text "Dostępne paczki tekstur: " 10 10 24 +color-white+)

      (funcall b1 (+ 10 wt) 10 #'switch-textures-to)
      (funcall b2 (+ 10 wt w1 20) 10 #'switch-textures-to)

      (end-drawing))
    ))

(defun maybe-configure-menu (&rest r)
  (declare (ignore r))
  (when (key-pressed-p-1 256)
    (end-drawing)
    (configure-menu)))

(add-draw-hook 'maybe-configure-menu)
