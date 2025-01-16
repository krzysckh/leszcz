(defpackage :leszcz
  (:use :common-lisp :cffi)
  (:export
   :main))

(in-package :leszcz)

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

(defcfun "BeginDrawing" :void)
(defcfun "EndDrawing" :void)

;; TODO: rename to kebab-case
(defcfun "InitWindow" :void
  (width :int)
  (height :int)
  (title :string))

(defcfun "CloseWindow" :void)

(defcfun "WindowShouldClose" :bool)

(defcfun "ClearBackground" :bool
  (color (:struct color)))

(defcfun "DrawText" :void
  (text :string)
  (x :int)
  (y :int)
  (font-size :int)
  (color (:struct color)))

(defun type-color-p (l)
  (and (listp l) (= (length l) 4)))

;; todo: ?
(deftype color ()
  '())

(defun main (argv)
  (declare (ignore argv))

  (initwindow 600 600 "*hello*")

  (loop :while (not (windowshouldclose)) :do
    (begindrawing)
    (clearbackground '(255 255 255 255))
    (drawtext "elo pozdro" 10 10 32 '(0 0 0 255))
    (enddrawing))

  (closewindow))
