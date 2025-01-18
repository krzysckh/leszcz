(defpackage :leszcz
  (:use :common-lisp :raylib :alexandria :cl-ppcre)
  (:export
   :main))

(in-package :leszcz)

(defun hasp (el l)
  (member el l :test #'equal))

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
(defparameter +color-purple+ '(200 0 200 255))
(defparameter +color-grayish+ '(127 127 127 255))
(defparameter +color-greenish+ '(0 200 0 128))

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

(define-constant +rook-moves+
    '((1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0)
      (-1 0) (-2 0) (-3 0) (-4 0) (-5 0) (-6 0) (-7 0) (-8 0)
      (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8)
      (0 -1) (0 -2) (0 -3) (0 -4) (0 -5) (0 -6) (0 -7) (0 -8))
  :test #'equal)

(define-constant +bishop-moves+
    '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8)
      (1 -1) (2 -2) (3 -3) (4 -4) (5 -5) (6 -6) (7 -7) (8 -8)
      (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7) (-8 8)
      (-1 -1) (-2 -2) (-3 -3) (-4 -4) (-5 -5) (-6 -6) (-7 -7) (-8 -8))
  :test #'equal)

(define-constant piece-move-alist
    `((pawn (0 1) (0 2) (1 1) (-1 1))
      (rook ,@+rook-moves+)
      (bishop ,@+bishop-moves+)
      (queen ,@(append +bishop-moves+ +rook-moves+))
      (king (1 0) (-1 0) (0 1) (0 -1) (-1 -1) (1 -1) (-1 1) (1 1))
      (knight (1 2) (1 -2) (-1 2) (-1 -2)
              (2 1) (-2 1) (2 -1) (-2 -1))
      )
  :test #'equal)

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
(defun possible-moves-for (p)
  (mapcar
   #'(lambda (pos)
       (let ((v (if (whitep p)
                    (list (- (car pos)) (- (cadr pos)))
                    pos)))
         (list (+ (car v) (point-x (piece-point p)))
               (+ (cadr v) (point-y (piece-point p))))))
   (cdr (assoc (piece-type p) piece-move-alist))))

(defun move-possible-p (p px py game)
  (let ((p* (piece-at-point game px py)))
    (and
     (hasp (list px py) (possible-moves-for p))
     (or
      (null p*)
      (not (eq (piece-color p) (piece-color p*)))))))

(defparameter maybe-drag/piece nil)
(defun maybe-drag (game)
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (cond
      ((mouse-pressed-p 0)  ; begin dragging
       (when-let ((p (piece-at-point game px py)))
         (setf maybe-drag/piece p)))
      ((and (mouse-released-p 0) maybe-drag/piece); end dragging
       (when (move-possible-p maybe-drag/piece px py game)
         (setf (piece-point maybe-drag/piece)
               (make-instance 'point :x px :y py)))
       (setq maybe-drag/piece nil))
      (maybe-drag/piece
       (draw-rectangle
        (* +piece-size+ (point-x (piece-point maybe-drag/piece)))
        (* +piece-size+ (point-y (piece-point maybe-drag/piece)))
        +piece-size+
        +piece-size+
        '(80 80 80 129))
       (draw-rectangle
        (* +piece-size+ px)
        (* +piece-size+ py)
        +piece-size+
        +piece-size+
        '(80 80 80 80))))))

(defun highlight-possible-moves (&optional game)
  (when-let ((p maybe-drag/piece))
    (let ((px (point-x (piece-point p)))
          (py (point-y (piece-point p))))
      (dolist (pos (possible-moves-for p))
          (draw-rectangle
           (* +piece-size+ (car pos))
           (* +piece-size+ (cadr pos))
           +piece-size+
           +piece-size+
           +color-greenish+)))))

(push #'show-point-at-cursor mainloop-draw-hooks)
(push #'maybe-drag mainloop-draw-hooks)
(push #'highlight-possible-moves mainloop-draw-hooks)

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
