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

(defcfun ("BeginDrawing" begin-drawing) :void)
(defcfun ("EndDrawing" end-drawing) :void)

(defcfun ("InitWindow" init-window) :void
  (width :int)
  (height :int)
  (title :string))

(defcfun ("CloseWindow" close-window) :void)

(defcfun ("WindowShouldClose" window-close-p) :bool)


(defcfun ("ClearBackground" clear-background) :bool
  (color (:struct color)))

(defcfun ("DrawText" draw-text) :void
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

(defclass point ()
  ((x
    :initarg :x
    :accessor point-x)
   (y
    :initarg :y
    :accessor point-y)))

(defclass piece ()
  ((type
    :initarg :type
    :accessor piece-type)
   (point
    :initarg :point
    :accessor piece-point)))

(defun piece->value (p)
  (declare (type piece p))
  (case (piece-type p)
    (king 1000)
    (queen 9)
    (rook 5)
    (bishop 3)
    (knight 3)
    (pawn 1)))

(defconstant +piece-size+ 32)
(defparameter +color-white+ '(255 255 255 255))
(defparameter +color-black+ '(0 0 0 255))

(defun draw-piece (p)
  (declare (type piece p))
  (let* ((point (piece-point p))
         (x (* +piece-size+ (point-x point)))
         (y (* +piece-size+ (point-y point))))
    (draw-text (string (char (string (piece-type p)) 0)) x y +piece-size+ '(0 0 0 255))))

(defparameter example-queen
  (make-instance
   'piece
   :point (make-instance 'point :x 1 :y 0)
   :type 'queen))

(defun main (&optional argv)
  (declare (ignore argv))

  (init-window (* +piece-size+ 8) (* +piece-size+ 8) "hello")

  (loop :while (not (window-close-p)) :do
    (begin-drawing)
    (clear-background +color-white+)
    (draw-piece example-queen)
    (end-drawing))

  (close-window))
