(defpackage :leszcz-types
  (:use :common-lisp :leszcz-constants :local-time)
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
   game-time-black
   game-time-white
   game-time-begin-turn

   ;; copiers
   copy-piece
   copy-game
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
   (time-begin-turn ; utc0 timestamp of beginning of turn next-time -= local-time:now - this
    :initarg :time-begin-turn
    :initform (local-time:timestamp-to-unix (local-time:now))
    :accessor game-time-begin-turn)
   (time-white ; time left (in seconds) for the game
    :initarg :time-white
    :initform 3600
    :accessor game-time-white)
   (time-black
    :initarg :time-black
    :initform 3600
    :accessor game-time-black)
  ))

(defmethod game-tick ((g game))
  (incf (the fixnum (game-ticker g))))

(defmethod game-in-progress-p ((g game))
  (eq (game-result g) 'in-progress))

(defmethod game-turn ((g game))
  (if (= (mod (the fixnum (game-ticker g)) 2) 0) 'white 'black))

(defmethod game-turn-white-p ((g game))
  (eq (game-turn g) 'white))

(defmethod game-turn-black-p ((g game))
  (eq (game-turn g) 'black))

;;; Printers

(defmethod print-object ((p piece) s)
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (format s "point(~a ~a)" (point-x p) (point-y p)))

(defmethod print-object ((g game) s)
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (format s "#<game instance with ~a pieces, turn=~a,tickers=(~a,~a,~a)>"
          (length (the list (game-pieces g)))
          (game-turn g)
          (game-ticker g)
          (game-halfmove-clock g)
          (game-fullmove-clock g)))

(defun whitep (p) (eq (piece-color p) 'white))
(defun blackp (p) (eq (piece-color p) 'black))

(defun copy-piece (p)
  (declare (type piece p))
  (the piece (make-instance
              'piece
              :type (piece-type p)
              :color (piece-color p)
              :point (make-instance 'point
                                    :x (point-x (piece-point p))
                                    :y (point-y (piece-point p))))))

(defun copy-game (g)
  (declare (type game g))
  (the game (make-instance
             'game
             :pieces (mapcar #'copy-piece (game-pieces g))
             :move-history (game-move-history g)
             :bcq-p (game-black-can-castle-queenside-p g)
             :bck-p (game-black-can-castle-kingside-p g)
             :wcq-p (game-white-can-castle-queenside-p g)
             :wck-p (game-white-can-castle-kingside-p g)
             :en-passant-target-square (game-en-passant-target-square g)
             :ticker (game-ticker g)
             :side (game-side g)
             :halfmove-clock (game-halfmove-clock g)
             :fullmove-clock (game-fullmove-clock g)
             :result (game-result g)
             :fb (game-fb g) ;; TODO: should i copy the thing here?
             :possible-moves-cache nil
             :points-cache nil
             :connection (game-connection g)
             :interactive-p (game-interactive-p g)
       )))
