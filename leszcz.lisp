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

(defparameter +texture-size+ 1024)

(defparameter white-texture-alist nil)
(defparameter black-texture-alist nil)

(defun piece->value (p)
  (declare (type piece p))
  (case (piece-type p)
    (king 1000)
    (queen 9)
    (rook 5)
    (bishop 3)
    (knight 3)
    (pawn 1)))

(defconstant +piece-size+ 64)
(defparameter +color-white+ '(255 255 255 255))
(defparameter +color-black+ '(0 0 0 255))
(defparameter +color-purple+ '(200 0 200 255))
(defparameter +color-grayish+ '(127 127 127 255))
(defparameter +color-greenish+ '(0 200 0 128))
(defparameter +color-redish+ '(200 30 0 128))

;; assuming a "vector" or "vector2" is a (list a b)
(defun v2+ (a b)
  (list
   (+ (car a) (car b))
   (+ (cadr a) (cadr b))))

(defun floatize (l)
  (mapcar #'float l))

(defun draw-piece (p)
  (declare (type piece p))

  (let* ((point (piece-point p))
         (x (* +piece-size+ (point-x point)))
         (y (* +piece-size+ (point-y point)))
         (al (if (whitep p) white-texture-alist black-texture-alist))
         (texture (cdr (assoc (piece-type p) al))))
    (draw-texture
     texture
     (floatize (list 0 0 +texture-size+ +texture-size+))
     (floatize (list x y +piece-size+ +piece-size+))
     (floatize (list 0 0))
     (float 0)
     +color-white+)))

(defparameter example-queen
  (make-instance
   'piece
   :point (make-instance 'point :x 1 :y 0)
   :color 'white
   :type 'queen))

;; can be <1..8> * vec ("sliding")
(define-constant +rook-offsets+ '((1 0) (-1 0) (0 1) (0 -1)) :test #'equal)
(define-constant +bishop-offsets+ '((1 1) (1 -1) (-1 1) (-1 -1)) :test #'equal)
(define-constant +queen-offsets+ (append +rook-offsets+ +bishop-offsets+) :test #'equal)

(define-constant +king-moves+ '((1 0) (-1 0) (0 1) (0 -1) (-1 -1) (1 -1) (-1 1) (1 1)) :test #'equal)
(define-constant +knight-moves+ '((1 2) (1 -2) (-1 2) (-1 -2) (2 1) (-2 1) (2 -1) (-2 -1)) :test #'equal)
;; not defining pawn moves because they "depend" :3

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

(defparameter +color-bg-light+ '(#xeb #xec #xd0 #xff))
(defparameter +color-bg-dark+  '(#x73 #x95 #x52 #xff))

(defun draw-game (g)
  (declare (type game g))
  (let ((i 0))
    (loop for y from 0 to 7 do
      (loop for x from 0 to 8 do
        (let ((color (if (= (mod i 2) 0) +color-bg-light+ +color-bg-dark+)))
          (draw-rectangle (* +piece-size+ x) (* +piece-size+ y) +piece-size+ +piece-size+ color)
          (incf i)))))

  (let ((pieces (game-pieces g)))
    (dolist (p pieces)
      (draw-piece p))))

(defun coord->value (v)
  (floor (/ v +piece-size+)))

(defun coords->point (x y)
  (values
   (coord->value x)
   (coord->value y)))

(defun position-of (p)
  (declare (type piece p))
  (values
   (point-x (piece-point p))
   (point-y (piece-point p))))

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

(defun filter-own-pieces (game p move-list)
  (remove-if
   #'(lambda (pos)
       (when-let ((p* (piece-at-point game (car pos) (cadr pos))))
         (eq (piece-color p*) (piece-color p))))
   move-list))

;; i used 100% of my brain for this name
(defun enposition-moveset (position moveset)
  (mapcar #'(lambda (p) (v2+ p position)) moveset))

(defun generate-sliding-moves (game p moveset)
  (declare (type game game)
           (type piece p))

  (multiple-value-bind (x y)
      (position-of p)
    (let ((acc nil))
      (loop for m in moveset do
        (block brk
          (loop for i from 1 to 8 do
            (let* ((x* (+ x (* i (car m))))
                   (y* (+ y (* i (cadr m))))
                   (p* (piece-at-point game x* y*)))
              (cond
                ((< x* 0)  (return-from brk))
                ((< y* 0)  (return-from brk))
                ((>= x* 8) (return-from brk))
                ((>= y* 8) (return-from brk))
                ((and p* (eq (piece-color p) (piece-color p*))) ; same-colored piece, no more moves
                 (return-from brk))
                (p* ; diff-colored piece, can be captured but no more
                 (push `(,x* ,y*) acc)
                 (return-from brk))
                (t ; empty square, can go there and possibly further
                 (push `(,x* ,y*) acc)))))))
      acc)))

(defun possible-moves-for (game p)
  (declare (type piece p))

  (multiple-value-bind (x y)
      (position-of p)
    (case (piece-type p)
      (pawn   (list (v2+ (list x y) (list 0 (if (whitep p) -1 1)))))
      (knight (filter-own-pieces game p (enposition-moveset (list x y) +knight-moves+)))
      (king   (filter-own-pieces game p (enposition-moveset (list x y) +king-moves+)))
      (rook   (generate-sliding-moves game p +rook-offsets+))
      (bishop (generate-sliding-moves game p +bishop-offsets+))
      (queen  (generate-sliding-moves game p +queen-offsets+))
      (t
       (warn "unreachable reached D:")))))

(defun move-possible-p (p px py game)
  (hasp (list px py) (possible-moves-for game p)))

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
         ;; assuming move impossible if piece at point is same color
         (when-let ((p (piece-at-point game px py)))
           (setf (game-pieces game)
                 (remove p (game-pieces game) :test #'equal)))
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

(defun highlight-possible-moves (game)
  (declare (type game game))

  (when-let ((p maybe-drag/piece))
    (let ((px (point-x (piece-point p)))
          (py (point-y (piece-point p))))
      (dolist (pos (possible-moves-for game p))
          (draw-rectangle
           (* +piece-size+ (car pos))
           (* +piece-size+ (cadr pos))
           +piece-size+
           +piece-size+
           +color-redish+)))))

(defun add-draw-hook (fn)
  (push fn mainloop-draw-hooks))

(defun remove-draw-hook (name)
  (setf mainloop-draw-hooks (remove name mainloop-draw-hooks)))

(add-draw-hook 'show-point-at-cursor)
(add-draw-hook 'maybe-drag)
(add-draw-hook 'highlight-possible-moves)

(defun load-textures ()
  (dolist (e white-texture-data-list)
    (push (cons (car e) (make-texture (cdr e) ".png")) white-texture-alist))
  (dolist (e black-texture-data-list)
    (push (cons (car e) (make-texture (cdr e) ".png")) black-texture-alist))
  (format t "loaded textures~%"))

(defun main (&optional argv)
  (declare (ignore argv))

  (init-window (* +piece-size+ 8) (* +piece-size+ 8) "hello")
  (set-target-fps! 30)

  (load-textures)

  (format t "white-texture-alist: ~a~%" white-texture-alist)
  (format t "black-texture-alist: ~a~%" black-texture-alist)

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
