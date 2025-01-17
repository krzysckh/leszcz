(defpackage :leszcz
  (:use :common-lisp :raylib :alexandria :cl-ppcre)
  (:export
   :main))

(in-package :leszcz)

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
   (color
    :initarg :color
    :accessor piece-color)
   (castle-p
    :initarg :castle-p
    :accessor piece-can-castle-p)
   (point
    :initarg :point
    :accessor piece-point)))

(defclass game ()
  ((pieces
    :initarg :pieces
    :accessor game-pieces)
   (move-of
    :initarg :move-of
    :accessor game-move-of)
   (move-history
    :initarg :move-history
    :accessor game-move-history)))

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
(defparameter +color-grayish+ '(127 127 127 255))

(defun draw-piece (p)
  (declare (type piece p))
  (let* ((point (piece-point p))
         (x (* +piece-size+ (point-x point)))
         (y (* +piece-size+ (point-y point)))
         (c (if (eq (piece-color p) 'white) +color-white+ +color-black+))
         (sym (if (eq (piece-type p) 'knight)
                  "N"
                  (string (char (string (piece-type p)) 0)) )))
    (draw-text sym x y +piece-size+ c)))

(defparameter example-queen
  (make-instance
   'piece
   :point (make-instance 'point :x 1 :y 0)
   :color 'white
   :type 'queen))

(defun fen->game (fen)
  (declare (type string fen))

  (let* ((l (split "\\s" fen))
         (fens (nth 0 l))
         (acc nil)
         (x 0)
         (y 0))
    (dolist (c (coerce fens 'list))
      (let ((color (if (lower-case-p c) 'black 'white))
            (type (case (char-downcase c)
                    (#\p 'pawn)
                    (#\r 'rook)
                    (#\n 'knight)
                    (#\b 'bishop)
                    (#\q 'queen)
                    (#\k 'king))))
        (cond
          (type
           (prog1 (push
                   (make-instance
                    'piece
                    :point (make-instance 'point :x x :y y)
                    :color color
                    :castle-p nil
                    :type type)
                   acc)
             (incf x)))
          ((eq c #\/)
           (setf x 0)
           (incf y))
          (t
           (incf x (- (char-int c) (char-int #\0)))))))
    ;; TODO: king castling, move count, last move
    (make-instance
     'game
     :pieces acc
     :move-of (if (equal (nth 1 l) "w") 'white 'black)
     :move-history nil)))

(define-constant *initial-fen* "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" :test #'equal)

(defparameter mainloop-draw-hooks nil)

(defun draw-game (g)
  (declare (type game g))
  (let ((pieces (game-pieces g)))
    (dolist (p pieces)
      (draw-piece p))))

(defun coord->value (v)
  (floor (/ v +piece-size+)))

(defun coords->point (x y)
  (values
   (coord->value x)
   (coord->value y)))

(defun show-point-at-cursor (&optional game)
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (draw-rectangle-lines (* px +piece-size+) (* py +piece-size+) +piece-size+ +piece-size+ +color-black+)))

(defun piece-at-point (game x y)
  (let ((pieces (game-pieces game)))
    (dolist (p pieces)
      (when (and (= (point-x (piece-point p)) x)
                 (= (point-y (piece-point p)) y))
        (return-from piece-at-point p)))
    nil))

(defun whitep (p) (eq (piece-color p) 'white))
(defun blackp (p) (eq (piece-color p) 'black))

(defun maybe-move1 (game)
  (when (mouse-pressed-p 0)
    (multiple-value-bind (px py)
        (coords->point (mouse-x) (mouse-y))
      (when-let ((p (piece-at-point game px py)))
        (setf (piece-point p)
              (make-instance
               'point
               :x (point-x (piece-point p))
               :y (+ (point-y (piece-point p))
                     (if (whitep p) -1 1))))))))

(push #'show-point-at-cursor mainloop-draw-hooks)
(push #'maybe-move1 mainloop-draw-hooks)

(defun main (&optional argv)
  (declare (ignore argv))

  (init-window (* +piece-size+ 8) (* +piece-size+ 8) "hello")
  (set-target-fps! 30)

  (let ((game (fen->game *initial-fen*)))
    (loop :while (not (window-close-p)) :do
      (begin-drawing)
      ;; in a progn to show block
      (progn
        (clear-background +color-grayish+)
        (draw-game game)
        (dolist (h mainloop-draw-hooks)
          (funcall h game)))
      (end-drawing)))

  (close-window))
