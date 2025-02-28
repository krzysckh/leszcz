(defpackage :raylib
  (:use :common-lisp :cffi :alexandria :leszcz-constants)
  (:export
   ;; Functions and whatnot
   load-textures

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
   mouse-pos-1
   draw-rectangle-lines
   draw-rectangle-lines-2
   draw-rectangle
   draw-rectangle-rounded
   clear-background
   draw-text
   draw-text-alagard
   draw-text-alagard-centered
   draw-text-centered
   draw-text-1
   draw-text-2
   draw-line
   draw-line-1
   draw-circle
   mouse-pressed-p
   mouse-down-p
   mouse-released-p
   load-image-from-memory
   load-font-from-memory
   image->texture
   texture->image
   make-texture
   make-font
   draw-texture
   set-exit-key!
   set-texture-filter!
   floatize
   get-char-pressed
   get-chars-pressed
   get-chars-pressed-1
   key-pressed-p
   key-pressed-p-1
   key-down-p
   key-down-p-1
   draw-fps
   load-font
   begin-scissor-mode
   end-scissor-mode
   measure-text
   measure-text-1
   point-in-rect-p
   set-mouse-cursor!
   screen->image
   unload-texture!
   unload-image!
   window-ready-p
   begin-texture-mode
   end-texture-mode
   make-render-texture
   unload-render-texture!
   scroll-delta
   open-url!

   ;; Constants
   +TEXTURE-FILTER-POINT+
   +TEXTURE-FILTER-BILINEAR+
   +TEXTURE-FILTER-TRILINEAR+
   +TEXTURE-FILTER-ANISOTROPIC-4X+
   +TEXTURE-FILTER-ANISOTROPIC-8X+
   +TEXTURE-FILTER-ANISOTROPIC-16X+

   +cursor-pointer+
   +cursor-normal+

   +key-escape+

   ;; exported variables
   *font*
   *alagard*
   ))

(in-package :raylib)

(declaim #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

(define-foreign-library raylib
  (:unix    "./raylib5.5.so")
  (:windows "raylib5.5.dll"))

(defun floatize (l)
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (mapcar #'float l))

(use-foreign-library raylib)

(defconstant +TEXTURE-FILTER-POINT+ 0)
(defconstant +TEXTURE-FILTER-BILINEAR+ 1)
(defconstant +TEXTURE-FILTER-TRILINEAR+ 2)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-4X+ 3)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-8X+ 4)
(defconstant +TEXTURE-FILTER-ANISOTROPIC-16X+ 5)

(defconstant +cursor-pointer+ 4)
(defconstant +cursor-normal+ 0)

(defconstant +key-escape+ 256)

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

(defcstruct (render-texture :class type-render-texture)
  (id :uint)
  (texture (:struct texture))
  (depth (:struct texture)))

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
;; (make-trans type-render-texture render-texture (id texture depth))
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

(defcfun ("LoadImageFromTexture" texture->image) (:struct image)
  (img (:struct texture)))

(defcfun ("LoadRenderTexture" make-render-texture) (:struct render-texture)
  (width :int)
  (height :int))

(defcfun ("UnloadRenderTexture" unload-render-texture!) :void
  (rt (:struct render-texture)))

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
    (load-font-from-memory data-type data (length font-data) font-size (cffi:null-pointer) n-codepoints)))

(defcfun ("DrawTexturePro" draw-texture) :void
  (texture (:struct texture))
  (src (:struct rectangle))
  (dst (:struct rectangle))
  (origin (:struct vec2))
  (rotation :float)
  (tint (:struct color)))

(defcfun ("BeginDrawing" begin-drawing-1) :void)
(defcfun ("EndDrawing" end-drawing-1) :void)
(defcfun ("SetTargetFPS" set-target-fps!) :void (fps :int))

(defun begin-drawing ()
  (begin-drawing-1)
  (set-mouse-cursor! +cursor-normal+ :begin t))

(defun end-drawing ()
  (set-mouse-cursor! +cursor-normal+ :finalize t)
  (end-drawing-1))

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

(defun mouse-pos-1 ()
  (list (mouse-x) (mouse-y)))

(defcfun ("DrawRectangleLines" draw-rectangle-lines) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (c (:struct color)))

(defcfun ("DrawRectangleRounded" draw-rectangle-rounded) :void
  (rec (:struct rectangle))
  (roundness :float)
  (segments :int)
  (color (:struct color)))

(defcfun ("DrawRectangleLinesEx" draw-rectangle-lines-2) :void
  (rec (:struct rectangle))
  (thick :float)
  (color (:struct color)))

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

(defcfun ("DrawLineEx" draw-line-1) :void
  (start (:struct vec2))
  (end (:struct vec2))
  (thick :float)
  (color (:struct color)))

(defcfun ("DrawCircle" draw-circle) :void
  (x :int)
  (y :int)
  (rad :float)
  (color (:struct color)))

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

(defcfun ("IsKeyDown" key-down-p-1) :bool
  (c :int))

(defun key-pressed-p (ch)
  (key-pressed-p-1 (char-code ch)))

(defun key-down-p (ch)
  (key-down-p-1 (char-code ch)))

;; TODO: to można napisać ładniej
(defun get-chars-pressed-1 ()
  (declare (values list))
  (let ((acc nil))
    (block break
      (loop do
        (let ((c (get-char-pressed)))
          (if (= c 0)
              (return-from break)
              (setf acc (append acc (list (code-char c))))))))
    acc))

(defun get-chars-pressed ()
  (declare (values (vector character)))
  (coerce (get-chars-pressed-1) '(vector character)))

(defparameter *font* (make-hash-table :test #'equal))
(defparameter *alagard* (make-hash-table :test #'equal))

(defun load-font (data size &key (type ".otf") (loaded-font-hash *font*))
  (if-let ((f (gethash size loaded-font-hash)))
    f
    (progn
      (setf (gethash size loaded-font-hash) (make-font data type size 1024))
      (gethash size loaded-font-hash))))

(defun draw-text (text x y font-size color)
  (let ((font (load-font spleen-data font-size)))
    (draw-text-2 font text (floatize (list x y)) (float font-size) 0.0 color)))

(defun draw-text-alagard (text x y font-size color)
  (let ((font (load-font alagard-data font-size :loaded-font-hash *alagard*)))
    (draw-text-2 font text (floatize (list x y)) (float font-size) 0.0 color)))
      ;; (draw-text-1 text (float x) (float y) (float font-size) color)))

;; centered only on x axis!
(defun draw-text-alagard-centered (text center-x y font-size color)
  (let-values ((font (load-font alagard-data font-size :loaded-font-hash *alagard* :type ".ttf"))
               (w h (measure-text-1 font text (float font-size) 0.0)))
    (draw-text-2 font text (floatize (list (- center-x (/ w 2)) y)) (float font-size) 0.0 color)))

(defun draw-text-centered (text center-x y font-size color)
  (let-values ((font (load-font spleen-data font-size))
               (w h (measure-text-1 font text (float font-size) 0.0)))
    (draw-text-2 font text (floatize (list (- center-x (/ w 2)) y)) (float font-size) 0.0 color)))

(defcfun ("IsMouseButtonPressed" mouse-pressed-p) :bool
  (b :int))

(defcfun ("IsMouseButtonDown" mouse-down-p) :bool
  (b :int))

(defcfun ("IsMouseButtonReleased" mouse-released-p) :bool
  (b :int))

(defcfun ("SetExitKey" set-exit-key!) :void
  (k :int))

(defcfun ("OpenURL" open-url!) :void
  (s :string))

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

(defcfun ("DrawFPS" draw-fps) :void
  (x :int)
  (y :int))

(defcfun ("BeginScissorMode" begin-scissor-mode) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defcfun ("EndScissorMode" end-scissor-mode) :void)

(defcfun ("GetMouseWheelMove" scroll-delta) :float)

(defcfun ("MeasureTextEx" measure-text) (:struct vec2)
  (font (:struct font))
  (text :string)
  (font-size :float)
  (spacing :float))

(defun measure-text-1 (font text font-size spacing)
  (let ((v (measure-text font text font-size spacing)))
    (values (car v) (cadr v))))

(defcfun ("CheckCollisionPointRec" point-in-rect-p) :bool
  (point (:struct vec2))
  (rec (:struct rectangle)))

(defcfun ("SetMouseCursor" set-mouse-cursor-1!) :void
  (cursor :int))

(defparameter smc/current +cursor-normal+)
(defun set-mouse-cursor! (cursor &key begin finalize)
  (cond
    (begin    (setf smc/current +cursor-normal+))
    (finalize (set-mouse-cursor-1! smc/current))
    (t
     (when (and (eq smc/current +cursor-normal+) (not (eq cursor +cursor-normal+)))
       (setf smc/current cursor)))))

(defcfun ("LoadImageFromScreen" screen->image) (:struct image))

(defcfun ("UnloadImage" unload-image!) :void
  (img (:struct image)))

(defcfun ("UnloadTexture" unload-texture!) :void
  (txt (:struct texture)))

(defcfun ("BeginTextureMode" begin-texture-mode) :void
  (txt (:struct render-texture)))

(defcfun ("EndTextureMode" end-texture-mode) :void)

(defcfun ("IsWindowReady" window-ready-p) :bool)

;; MOUSE_CURSOR_DEFAULT       = 0
;; MOUSE_CURSOR_ARROW         = 1
;; MOUSE_CURSOR_IBEAM         = 2
;; MOUSE_CURSOR_CROSSHAIR     = 3
;; MOUSE_CURSOR_POINTING_HAND = 4
;; MOUSE_CURSOR_RESIZE_EW     = 5
;; MOUSE_CURSOR_RESIZE_NS     = 6
;; MOUSE_CURSOR_RESIZE_NWSE   = 7
;; MOUSE_CURSOR_RESIZE_NESW   = 8
;; MOUSE_CURSOR_RESIZE_ALL    = 9
;; MOUSE_CURSOR_NOT_ALLOWED   = 10

(defun type-color-p (l)
  (and (listp l) (= (length l) 4)))

;; todo: ?
(deftype color ()
  '())

(defun load-textures ()
  ;; Draw two (2) frames
  (begin-drawing)
  (clear-background '(#x22 #x22 #x22 #xff))
  (draw-text-1 "loading textures..." 10 10 24 '(#xde #xde #xde #xff)) ; basic raylib font
  (end-drawing)

  (begin-drawing)
  (clear-background '(#x22 #x22 #x22 #xff))
  (draw-text "loading textures..." 10 10 24 '(#xde #xde #xde #xff)) ; spleen, this takes some time to load though
  (end-drawing)

  (setf white-texture-alist nil)
  (setf black-texture-alist nil)
  (setf leszcz-logos-alist nil)
  (setf icon-texture-alist nil)
  (setf raylib:*font* (make-hash-table :test #'equal)) ;; reset *font* every texture reload
  (setf raylib:*alagard* (make-hash-table :test #'equal)) ;; reset *alagard* every texture reload

  ;; (setf raylib:*font* (make-font spleen-data ".otf" 18 1024))
  (raylib:load-font spleen-data 18)
  ;; TODO:
  ;; ;; TODO: czemu tekstury są tak rozpikselizowane lol
  ;; (set-texture-filter! texture +TEXTURE-FILTER-POINT+)
  (macrolet ((load* (data-list alist)
               `(dolist (e ,data-list)
                  (if (listp (cdr e))
                      (let ((textures (mapcar #'(lambda (data) (make-texture data ".png")) (cdr e))))
                        (push (cons (car e) (coerce textures 'vector)) ,alist))
                      (let ((texture (make-texture (cdr e) ".png")))
                        (push (cons (car e) texture) ,alist))))))
    (load* white-texture-data-list white-texture-alist)
    (load* black-texture-data-list black-texture-alist)
    (load* logo-data-alist leszcz-logos-alist)
    (load* icon-data-alist icon-texture-alist)

    (format t "loaded textures~%")))
