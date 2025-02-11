(defpackage :leszcz-constants
  (:use :common-lisp :alexandria)
  (:export
   load-textures
   white-texture-data-list
   black-texture-data-list
   white-texture-alist
   black-texture-alist
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
   texture-options
   let-values

   ;; symbols
   pawn
   rook
   knight
   bishop
   queen
   king
   white
   black

   sleek
   pixel

   in-progress
   checkmate
   draw
   ))


(in-package :leszcz-constants)

(defun file->vec (fname)
  (let* ((f (open fname :element-type '(unsigned-byte 8)))
         (n (file-length f))
         (vec (make-sequence 'vector n)))
    (read-sequence vec f)
    (close f)
    vec))

(defparameter white-texture-data-list-sleek
  (list
   (cons 'pawn   (file->vec "res/png/pl.png"))
   (cons 'rook   (file->vec "res/png/rl.png"))
   (cons 'knight (file->vec "res/png/nl.png"))
   (cons 'bishop (file->vec "res/png/bl.png"))
   (cons 'queen  (file->vec "res/png/ql.png"))
   (cons 'king   (file->vec "res/png/kl.png"))))

(defparameter black-texture-data-list-sleek
  (list
   (cons 'pawn   (file->vec "res/png/pd.png"))
   (cons 'rook   (file->vec "res/png/rd.png"))
   (cons 'knight (file->vec "res/png/nd.png"))
   (cons 'bishop (file->vec "res/png/bd.png"))
   (cons 'queen  (file->vec "res/png/qd.png"))
   (cons 'king   (file->vec "res/png/kd.png"))))

;; TODO: skiny jak szymon zaimplementuje menu
(defparameter white-texture-data-list-pixel
  (list
   ;; (cons 'pawn   (file->vec "res/png/pl.png"))
   (cons 'pawn   (list
                  (file->vec "res/png/pl1.png")
                  (file->vec "res/png/pl2.png")
                  (file->vec "res/png/pl1.png")
                  (file->vec "res/png/pl3.png")))
   ;; (cons 'rook   (file->vec "res/png/rl.png"))
   (cons 'rook   (list
                  (file->vec "res/png/rl1.png")
                  (file->vec "res/png/rl2.png")
                  (file->vec "res/png/rl1.png")
                  (file->vec "res/png/rl3.png")))
   ;; (cons 'knight (file->vec "res/png/nl.png"))
   (cons 'knight (list
                  (file->vec "res/png/nl1.png")
                  (file->vec "res/png/nl2.png")
                  (file->vec "res/png/nl3.png")
                  (file->vec "res/png/nl4.png")
                  (file->vec "res/png/nl3.png")
                  (file->vec "res/png/nl2.png")))
   ;; (cons 'bishop (file->vec "res/png/bl.png"))
   (cons 'bishop (list
                  (file->vec "res/png/bl1.png")
                  (file->vec "res/png/bl2.png")
                  (file->vec "res/png/bl3.png")
                  (file->vec "res/png/bl2.png")))
   ;; (cons 'queen  (file->vec "res/png/ql.png"))
   (cons 'queen  (list
                  (file->vec "res/png/ql1.png")
                  (file->vec "res/png/ql2.png")))
   ;; (cons 'king   (file->vec "res/png/kl.png"))))
   (cons 'king   (list
                  (file->vec "res/png/kl1.png")
                  (file->vec "res/png/kl2.png")
                  (file->vec "res/png/kl3.png")))))

(defparameter black-texture-data-list-pixel
  (list
   ;; (cons 'pawn   (file->vec "res/png/pd.png"))
   (cons 'pawn   (list
                  (file->vec "res/png/pd1.png")
                  (file->vec "res/png/pd2.png")
                  (file->vec "res/png/pd1.png")
                  (file->vec "res/png/pd3.png")))
   ;; (cons 'rook   (file->vec "res/png/rd.png"))
   (cons 'rook  (list
                  (file->vec "res/png/rd1.png")
                  (file->vec "res/png/rd2.png")
                  (file->vec "res/png/rd1.png")
                  (file->vec "res/png/rd3.png")))
   ;; (cons 'knight (file->vec "res/png/nd.png"))
   (cons 'knight (list
                  (file->vec "res/png/nd1.png")
                  (file->vec "res/png/nd2.png")
                  (file->vec "res/png/nd3.png")
                  (file->vec "res/png/nd4.png")
                  (file->vec "res/png/nd3.png")
                  (file->vec "res/png/nd2.png")))
   ;; (cons 'bishop (file->vec "res/png/bd.png"))
   (cons 'bishop (list
                  (file->vec "res/png/bd1.png")
                  (file->vec "res/png/bd2.png")
                  (file->vec "res/png/bd3.png")
                  (file->vec "res/png/bd2.png")))
   ;; (cons 'queen  (file->vec "res/png/qd.png"))
   (cons 'queen  (list
                  (file->vec "res/png/qd1.png")
                  (file->vec "res/png/qd2.png")))
   ;; (cons 'king   (file->vec "res/png/kd.png"))))
   (cons 'king   (list
                  (file->vec "res/png/kd1.png")
                  (file->vec "res/png/kd2.png")
                  (file->vec "res/png/kd3.png")))))

(defparameter white-texture-data-list white-texture-data-list-pixel)
(defparameter black-texture-data-list black-texture-data-list-pixel)

(defparameter texture-options
  `((sleek (white . ,white-texture-data-list-sleek) (black . ,black-texture-data-list-sleek))
    (pixel (white . ,white-texture-data-list-pixel) (black . ,black-texture-data-list-pixel))))

(defparameter spleen-data (file->vec "res/font/spleen-16x32.otf"))

(defparameter +texture-size+ 1024)
(defparameter +piece-size+ 64)
(defparameter +color-white+ '(255 255 255 255))
(defparameter +color-black+ '(0 0 0 255))
(defparameter +color-purple+ '(200 0 200 255))
(defparameter +color-grayish+ '(127 127 127 255))
(defparameter +color-greenish+ '(0 200 0 128))
(defparameter +color-redish+ '(200 30 0 128))

;; (defparameter +color-bg-light+ '(#xeb #xec #xd0 #xff))
;; (defparameter +color-bg-dark+  '(#x73 #x95 #x52 #xff))

(defparameter +color-bg-light+ '(#xde #xde #xde #xff))
(defparameter +color-bg-dark+  '(#x22 #x22 #x22 #xff))

(defparameter *window-width*  (* +piece-size+ 8))
(defparameter *window-height* (* +piece-size+ 8))

(define-constant +initial-fen+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" :test #'equal)

(defparameter white-texture-alist nil)
(defparameter black-texture-alist nil)

(defmacro let-values (bindings &body b)
  (let ((l b)
        (listp nil))
    (loop for b in (reverse bindings) do
      (let ((v `(multiple-value-bind (,@(butlast b)) ,(car (last b)))))
        (setf l (append v (if listp (list l) l)))
        (setf listp t)))
    l))
