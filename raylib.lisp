(defpackage :raylib
  (:use :common-lisp :cffi)
  (:export
   color
   init-window
   close-window
   window-close-p
   begin-drawing
   end-drawing
   set-target-fps!
   mouse-x
   mouse-y
   mouse-pos
   draw-rectangle-lines
   clear-background
   draw-text
   draw-line
   mouse-pressed-p
   mouse-released-p
   ))

(in-package :raylib)

(define-foreign-library raylib
  (:unix    "raylib5.5.so")
  (:windows "raylib5.5.dll"))

(use-foreign-library raylib)

(defcstruct (color :class type-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defmethod translate-into-foreign-memory (l (type type-color) pointer)
  (with-foreign-slots ((r g b a) pointer (:struct color))
    (setf r (nth 0 l))
    (setf g (nth 1 l))
    (setf b (nth 2 l))
    (setf a (nth 3 l))))

(defmethod translate-from-foreign (pointer (type type-color))
  (with-foreign-slots ((r g b a) pointer (:struct color))
   (list r g b a)))

(defcfun ("BeginDrawing" begin-drawing) :void)
(defcfun ("EndDrawing" end-drawing) :void)
(defcfun ("SetTargetFPS" set-target-fps!) :void (fps :int))

(defcfun ("InitWindow" init-window) :void
  (width :int)
  (height :int)
  (title :string))

(defcfun ("CloseWindow" close-window) :void)

(defcfun ("WindowShouldClose" window-close-p) :bool)

(defcfun ("GetMouseX" mouse-x) :int)
(defcfun ("GetMouseY" mouse-y) :int)

(defun mouse-pos ()
  (values (mouse-x) (mouse-y)))

(defcfun ("DrawRectangleLines" draw-rectangle-lines) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (c (:struct color)))

(defcfun ("DrawLine" draw-line) :void
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int)
  (c (:struct color)))

(defcfun ("ClearBackground" clear-background) :bool
  (color (:struct color)))

(defcfun ("DrawText" draw-text) :void
  (text :string)
  (x :int)
  (y :int)
  (font-size :int)
  (color (:struct color)))

(defcfun ("IsMouseButtonPressed" mouse-pressed-p) :bool
  (b :int))

(defcfun ("IsMouseButtonReleased" mouse-released-p) :bool
  (b :int))

(defun type-color-p (l)
  (and (listp l) (= (length l) 4)))

;; todo: ?
(deftype color ()
  '())
