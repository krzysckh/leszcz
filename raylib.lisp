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
   load-font-from-memory
   image->texture
   make-texture
   make-font
   draw-texture
   set-exit-key!
   set-texture-filter!
   floatize
   get-char-pressed
   get-chars-pressed
   key-pressed-p

   ;; Constants
   +TEXTURE-FILTER-POINT+
   +TEXTURE-FILTER-BILINEAR+
   +TEXTURE-FILTER-TRILINEAR+
   +TEXTURE-FILTER-ANISOTROPIC-4X+
   +TEXTURE-FILTER-ANISOTROPIC-8X+
   +TEXTURE-FILTER-ANISOTROPIC-16X+

   ;; exported variables
   *font*
   ))

(in-package :raylib)

(define-foreign-library raylib
  (:unix    "./raylib5.5.so")
  (:windows "raylib5.5.dll"))

(defparameter *font* nil)

(defun floatize (l)
  (mapcar #'float l))

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

(defcstruct (glyph-info :class type-glyph-info)
  (value :int)
  (offX :int)
  (offY :int)
  (advX :int)
  (img (:struct image)))

(defcstruct (font :class type-font)
  (base-size :int)
  (glyph-count :int)
  (glyph-pad :int)
  (texture (:struct texture))
  (recs :pointer)
  (glyphs :pointer))

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
;; (make-trans type-glyph-info glyph-info (value offX offY advX img))
;; (make-trans type-font font (base-size glyph-count glyph-pad texture recs glyphs))

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

(defun make-font (font-data data-type font-size n-codepoints)
  (with-foreign-object (data :uint8 (length font-data))
    (loop for i from 0 to (- (length font-data) 1) do
      (setf (mem-aref data :uint8 i) (aref font-data i)))
    (the type-font (load-font-from-memory data-type data (length font-data) font-size (cffi:null-pointer) n-codepoints))))

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

(defcfun ("DrawText" draw-text-1) :void
  (text :string)
  (x :int)
  (y :int)
  (font-size :int)
  (color (:struct color)))

(defcfun ("DrawTextEx" draw-text-2) :void
  (font (:struct font))
  (text :string)
  (pos (:struct vec2))
  (font-size :float)
  (spacing :float)
  (color (:struct color)))

(defcfun ("GetCharPressed" get-char-pressed) :char)
(defcfun ("IsKeyPressed" key-pressed-p-1) :bool
  (c :int))

(defun key-pressed-p (ch)
  (key-pressed-p-1 (char-code ch)))

;; TODO: to można napisać ładniej
(defun get-chars-pressed ()
  (let ((acc nil))
    (block break
      (loop do
        (let ((c (get-char-pressed)))
          (if (= c 0)
              (return-from break)
              (setf acc (append acc (list (code-char c))))))))
    acc))

(defun draw-text (text x y font-size color)
  (if *font*
      (draw-text-2 *font* text (floatize (list x y)) (float font-size) 0.0 color)
      (draw-text-1 text (float x) (float y) (float font-size) color)))

(defcfun ("IsMouseButtonPressed" mouse-pressed-p) :bool
  (b :int))

(defcfun ("IsMouseButtonReleased" mouse-released-p) :bool
  (b :int))

(defcfun ("SetExitKey" set-exit-key!) :void
  (k :int))

(defcfun ("SetTextureFilter" set-texture-filter!) :void
  (texture (:struct texture))
  (filter :int))

(defcfun ("LoadFontFromMemory" load-font-from-memory) (:struct font)
  (type :string)
  (data :pointer)
  (data-size :int)
  (font-size :int)
  (codepoints :pointer)
  (codepoint-count :int))

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
