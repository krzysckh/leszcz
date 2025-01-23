(defpackage :raylib
  (:use :common-lisp :cffi)
  (:export
   ;; Functions and whatnot
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
   draw-rectangle
   clear-background
   draw-text
   draw-line
   mouse-pressed-p
   mouse-released-p
   load-image-from-memory
   image->texture
   make-texture
   draw-texture
   set-exit-key!
   set-texture-filter!

   ;; Constants
   +TEXTURE-FILTER-POINT+
   +TEXTURE-FILTER-BILINEAR+
   +TEXTURE-FILTER-TRILINEAR+
   +TEXTURE-FILTER-ANISOTROPIC-4X+
   +TEXTURE-FILTER-ANISOTROPIC-8X+
   +TEXTURE-FILTER-ANISOTROPIC-16X+
   ))

(in-package :raylib)

(define-foreign-library raylib
  (:unix    "./raylib5.5.so")
  (:windows "raylib5.5.dll"))

(use-foreign-library raylib)

(defcstruct (color :class type-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcstruct (image :class type-image)
  (data :pointer)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defcstruct (texture :class type-texture)
  (id :uint)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defcstruct (vec2 :class type-vec2)
  (x :float)
  (y :float))

(defcstruct (rectangle :class type-rectangle)
  (x :float)
  (y :float)
  (w :float)
  (h :float))

(defmacro make-trans (type stype slots)
  `(progn
     (defmethod translate-into-foreign-memory (l (type ,type) pointer)
       (with-foreign-slots (,slots pointer (:struct ,stype))
         ,@(loop for i from 0 to (- (length slots) 1)
                 collect (list 'setf (nth i slots) `(nth ,i l)))))

     (defmethod translate-from-foreign (pointer (type ,type))
       (with-foreign-slots (,slots pointer (:struct ,stype))
         (list ,@slots)))))

(make-trans type-color color (r g b a))
(make-trans type-image image (data width height mipmaps format))
(make-trans type-texture texture (id width height mipmaps format))
(make-trans type-vec2 vec2 (x y))
(make-trans type-rectangle rectangle (x y w h))

;; (defmethod translate-into-foreign-memory (l (type type-color) pointer)
;;   (with-foreign-slots ((r g b a) pointer (:struct color))
;;     (setf r (nth 0 l))
;;     (setf g (nth 1 l))
;;     (setf b (nth 2 l))
;;     (setf a (nth 3 l))))


(defcfun ("LoadImageFromMemory" load-image-from-memory) (:struct image)
  (type :string)
  (data :pointer)
  (data-size :int))

(defcfun ("LoadTextureFromImage" image->texture) (:struct texture)
  (img (:struct image)))

(defun make-texture (texture-data data-type)
  (with-foreign-object (data :uint8 (length texture-data))
    (loop for i from 0 to (- (length texture-data) 1) do
      (setf (mem-aref data :uint8 i) (aref texture-data i)))
    (let ((image (load-image-from-memory data-type data (length texture-data))))
      (image->texture image))))

(defcfun ("DrawTexturePro" draw-texture) :void
  (texture (:struct texture))
  (src (:struct rectangle))
  (dst (:struct rectangle))
  (origin (:struct vec2))
  (rotation :float)
  (tint (:struct color)))

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

(defcfun ("DrawRectangle" draw-rectangle) :void
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

(defcfun ("SetExitKey" set-exit-key!) :void
  (k :int))

(defcfun ("SetTextureFilter" set-texture-filter!) :void
  (texture (:struct texture))
  (filter :int))


(defconstant +TEXTURE-FILTER-POINT+ 0)
(defconstant +TEXTURE-FILTER-BILINEAR+ 1)
(defconstant +TEXTURE-FILTER-TRILINEAR+ 2)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-4X+ 3)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-8X+ 4)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-16X+ 5)

(defun type-color-p (l)
  (and (listp l) (= (length l) 4)))

;; todo: ?
(deftype color ()
  '())
