(defpackage :fast
  (:use :common-lisp :leszcz-constants :leszcz-types :alexandria :cl-ppcre)
  (:export
   fast-board
   fast-board-1

   bit-at
   bit-set-p
   game->fast-board
   fast-board->game

   fb-pawn
   fb-rook
   fb-knight
   fb-bishop
   fb-queen
   fb-king
   fb-white
   fb-black
   ))

(in-package :fast)

;;; TODO: to będzie biblioteka która po przekonwertowaniu typu `game' na
;; coś bardziej optymalnego będzie przyspieszała generowanie ruchów itp
;; lol


(defstruct (fast-board-1 (:conc-name fb-))
  (pawn   0 :type (unsigned-byte 64))
  (rook   0 :type (unsigned-byte 64))
  (knight 0 :type (unsigned-byte 64))
  (bishop 0 :type (unsigned-byte 64))
  (queen  0 :type (unsigned-byte 64))
  (king   0 :type (unsigned-byte 64)))

(defstruct (fast-board (:conc-name fb-))
  (black (make-instance 'fast-board-1) :type fast-board-1)
  (white (make-instance 'fast-board-1) :type fast-board-1)
  (ticker 0 :type fixnum)
  (wck-p t :type boolean)
  (wcq-p t :type boolean)
  (bck-p t :type boolean)
  (bcq-p t :type boolean))

(defmacro logbitpr (n bit &key (type-size 64))
  `(logbitp (- ,type-size 1 ,bit) ,n))

(defun game->fast-board (g)
  (declare (type game g))
  (let ((fb (make-instance 'fast-board)))
    (loop for p in (game-pieces g) do
      (let* ((b (if (whitep p) (fb-white fb) (fb-black fb)))
             (pt (piece-point p))
             (bit (+ (* (point-y pt) 8) (point-x pt))))
        (case (piece-type p)
          (pawn   (setf (logbitpr (fb-pawn b)   bit) t))
          (rook   (setf (logbitpr (fb-rook b)   bit) t))
          (bishop (setf (logbitpr (fb-bishop b) bit) t))
          (knight (setf (logbitpr (fb-knight b) bit) t))
          (queen  (setf (logbitpr (fb-queen b)  bit) t))
          (king   (setf (logbitpr (fb-king b)   bit) t))
          (t
           (error "Unknown piece-type ~a." (piece-type p))))))
    fb))

(defmacro fb-point (fb color-accessor piece-accessor x y)
  `(logbitpr
    (,piece-accessor (,color-accessor ,fb))
    (+ ,x (* 8 ,y))))

(defun fast-board->game (fb)
  (declare (type fast-board fb))
  (let ((acc nil))
    ;; manual unrolling lmao
    #.(append
       '(progn)
       (loop for t-accessor in '((fb-pawn pawn) (fb-rook rook)
                                 (fb-bishop bishop) (fb-knight knight)
                                 (fb-queen queen) (fb-king king))
             collect
             `(loop for y from 0 below 8 do
               (loop for x from 0 below 8 do
                 (when (fb-point fb fb-white ,(car t-accessor) x y)
                   (push (make-instance
                          'piece
                          :point (make-instance 'point :x x :y y)
                          :color 'white
                          :type ',(cadr t-accessor))
                         acc))
                 (when (fb-point fb fb-black ,(car t-accessor) x y)
                   (push (make-instance
                          'piece
                          :point (make-instance 'point :x x :y y)
                          :color 'black
                          :type ',(cadr t-accessor))
                         acc))))))
    (make-instance
     'game
     :pieces acc
     :ticker (fb-ticker fb)
     :move-history nil
     :wck-p (fb-wck-p fb)
     :wcq-p (fb-wcq-p fb)
     :bck-p (fb-bck-p fb)
     :bcq-p (fb-bcq-p fb)
     :en-passant-target-square nil
     ;; TODO: is en-passant target square needed?
     ; :en-passant-target-square (pos->lst (nth 3 l))
     )))

(defun fb-make-piece-board (fb)
  (declare (type fast-board fb))
  (logior
   (fb-pawn   (fb-white fb))
   (fb-rook   (fb-white fb))
   (fb-knight (fb-white fb))
   (fb-bishop (fb-white fb))
   (fb-queen  (fb-white fb))
   (fb-king   (fb-white fb))
   (fb-pawn   (fb-black fb))
   (fb-rook   (fb-black fb))
   (fb-knight (fb-black fb))
   (fb-bishop (fb-black fb))
   (fb-queen  (fb-black fb))
   (fb-king   (fb-black fb))))

(defun fb-display (n)
  (declare (type (unsigned-byte 64) n))
  (loop for i from 1 below 9 do
    (let ((x (logand #xff (ash n (- (- 64 (* 8 i)))))))
      (format t "~&~8,'0b~%" x))))

(defvar pawn-magic-L (lognot #x8080808080808080)) ;; pawns without far left side
(defvar pawn-magic-R (lognot #x0101010101010101))

;; Color of checker
(defun fb-make-check-board (fb color)
  (declare (type symbol color)
           (type fast-board fb))
  (let* ((whitep (eq color 'white))
         (f1 (if whitep (fb-white fb) (fb-black fb)))
         (pawn-+1 (if whitep 9 -9))
         (pawn-+2 (if whitep 7 -7)))
    (logior
     (ash (if whitep
              (logand pawn-magic-L (fb-pawn f1))
              (logand pawn-magic-R (fb-pawn f1)))
          pawn-+1) ;; pawn L
     (ash (if whitep
              (logand pawn-magic-R (fb-pawn f1))
              (logand pawn-magic-L (fb-pawn f1)))
          pawn-+2)))) ;; pawn R

(defun bit-at (n bit &key (type-size 64))
  (declare (type (unsigned-byte 64) n bit))
  (logand 1 (ash n (- (- type-size 1 bit)))))

(defun bit-set-p (n bit &key (type-size 64))
  (logbitp (- type-size 1 bit) n))
