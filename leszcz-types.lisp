(defpackage :leszcz-types
  (:use :common-lisp :leszcz-constants)
  (:export
   game
   point
   piece
   ;; point
   point-x
   point-y
   ;; piece
   piece-type
   piece-color
   piece-point
   whitep
   blackp
   ;; game (custom)
   game-tick
   game-turn
   game-turn-white-p
   game-turn-black-p
   game-in-progress-p
   ;; game
   game-pieces
   game-move-history
   game-black-can-castle-kingside-p
   game-black-can-castle-queenside-p
   game-white-can-castle-kingside-p
   game-white-can-castle-queenside-p
   game-en-passant-target-square
   game-ticker
   game-possible-moves-cache
   game-side
   game-points-cache
   game-halfmove-clock
   game-fullmove-clock
   game-result
   game-connection
   game-fb
   game-interactive-p

   ))

(in-package :leszcz-types)

(defclass point ()
  ((x
    :initarg :x
    :type fixnum
    :accessor point-x)
   (y
    :initarg :y
    :type fixnum
    :accessor point-y)))

(defclass piece ()
  ((type
    :initarg :type
    :accessor piece-type)
   (color
    :initarg :color
    :accessor piece-color)
   (point
    :initarg :point
    :accessor piece-point)
  ))

(defclass game ()
  ((pieces
    :initarg :pieces
    :accessor game-pieces)
   (move-history
    :initarg :move-history
    :accessor game-move-history)
   (black-can-castle-queenside-p
    :initarg :bcq-p
    :accessor game-black-can-castle-queenside-p)
   (black-can-castle-kingside-p
    :initarg :bck-p
    :accessor game-black-can-castle-kingside-p)
   (white-can-castle-queenside-p
    :initarg :wcq-p
    :accessor game-white-can-castle-queenside-p)
   (white-can-castle-kingside-p
    :initarg :wck-p
    :accessor game-white-can-castle-kingside-p)
   (en-passant-target-square
    :initarg :en-passant-target-square
    :accessor game-en-passant-target-square)
   (ticker
    :initarg :ticker
    :type fixnum 
    :initform (the fixnum 0)
    :accessor game-ticker)
   (possible-moves-cache
    :initarg :possible-moves-cache
    :initform nil
    :accessor game-possible-moves-cache)
   (side
    :initarg :side
    :accessor game-side)
   (points-cache
    :initarg :points-cache
    :initform nil
    :accessor game-points-cache)
   (halfmove-clock
    :initarg :halfmove-clock
    :initform 0
    :accessor game-halfmove-clock)
   (fullmove-clock
    :initarg :fullmove-clock
    :initform 1
    :accessor game-fullmove-clock)
   (result
    :initarg :result
    :initform 'in-progress
    :accessor game-result)
   (connection
    :initarg :connection
    :accessor game-connection)
   (fb
    :initarg :fb
    :accessor game-fb)
   (interactive-p
    :initarg :interactive-p
    :accessor game-interactive-p)
  ))

(defmethod game-tick ((g game))
  (incf (the fixnum (game-ticker g))))

(defmethod game-in-progress-p ((g game))
  (eq (game-result g) 'in-progress))

;;; Printers

(defmethod print-object ((p piece) s)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((type (if (slot-boundp p 'type)
                  (piece-type p)
                  'type-unknown))
        (color (if (slot-boundp p 'color)
                   (piece-color p)
                   'color-unknown))
        (point (if (slot-boundp p 'point)
                   (piece-point p)
                   'point-unknown)))
    (format s "piece(~a[~a])@~a" type color point)))

(defmethod print-object ((p point) s)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (format s "point(~a ~a)" (point-x p) (point-y p)))

(defmethod print-object ((g game) s)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (format s "#<game instance with ~a pieces, turn=~a,tickers=(~a,~a,~a)>"
          (length (the list (game-pieces g)))
          (game-turn g)
          (game-ticker g)
          (game-halfmove-clock g)
          (game-fullmove-clock g)))

(defun whitep (p) (eq (piece-color p) 'white))
(defun blackp (p) (eq (piece-color p) 'black))
