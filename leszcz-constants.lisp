(defpackage :leszcz-constants
  (:use :common-lisp :alexandria)
  (:export
   white-texture-data-list
   black-texture-data-list
   spleen-data
   +texture-size+
   +piece-size+
   +color-white+
   +color-black+
   +color-purple+
   +color-grayish+
   +color-greenish+
   +color-redish+
   +color-bg-light+
   +color-bg-dark+
   *window-height*
   *window-width*
   +initial-fen+
   pawn
   rook
   knight
   bishop
   queen
   king
   white
   black
   ))


(in-package :leszcz-constants)

(defun file->vec (fname)
  (let* ((f (open fname :element-type '(unsigned-byte 8)))
         (n (file-length f))
         (vec (make-sequence 'vector n)))
    (read-sequence vec f)
    (close f)
    vec))

(defparameter white-texture-data-list
  (list
   (cons 'pawn   (file->vec "res/png/pl.png"))
   (cons 'rook   (file->vec "res/png/rl.png"))
   (cons 'knight (file->vec "res/png/nl.png"))
   (cons 'bishop (file->vec "res/png/bl.png"))
   (cons 'queen  (file->vec "res/png/ql.png"))
   (cons 'king   (file->vec "res/png/kl.png"))))

(defparameter black-texture-data-list
  (list
   (cons 'pawn   (file->vec "res/png/pd.png"))
   (cons 'rook   (file->vec "res/png/rd.png"))
   (cons 'knight (file->vec "res/png/nd.png"))
   (cons 'bishop (file->vec "res/png/bd.png"))
   (cons 'queen  (file->vec "res/png/qd.png"))
   (cons 'king   (file->vec "res/png/kd.png"))))

(defparameter spleen-data (file->vec "res/font/spleen-16x32.otf"))

(defparameter +texture-size+ 1024)
(defconstant +piece-size+ 64)
(defparameter +color-white+ '(255 255 255 255))
(defparameter +color-black+ '(0 0 0 255))
(defparameter +color-purple+ '(200 0 200 255))
(defparameter +color-grayish+ '(127 127 127 255))
(defparameter +color-greenish+ '(0 200 0 128))
(defparameter +color-redish+ '(200 30 0 128))
(defparameter +color-bg-light+ '(#xeb #xec #xd0 #xff))
(defparameter +color-bg-dark+  '(#x73 #x95 #x52 #xff))

(defparameter *window-width*  (* +piece-size+ 8))
(defparameter *window-height* (* +piece-size+ 8))

(define-constant +initial-fen+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" :test #'equal)
