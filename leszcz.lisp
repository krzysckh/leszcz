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
   (point
    :initarg :point
    :accessor piece-point)))

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
    :initform 0
    :accessor game-ticker)
  ))

(defmethod game-tick ((g game))
  (incf (game-ticker g)))

(defmethod game-turn ((g game))
  (if (= (mod (game-ticker g) 2) 0) 'white 'black))

(defmethod game-turn-white-p ((g game))
  (eq (game-turn g) 'white))

(defmethod game-turn-black-p ((g game))
  (eq (game-turn g) 'black))

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
(defparameter +color-bg-light+ '(#xeb #xec #xd0 #xff))
(defparameter +color-bg-dark+  '(#x73 #x95 #x52 #xff))

(defparameter mainloop-draw-hooks nil)

;; assuming a "vector" or "vector2" is a (list a b)
(defun v2+ (a b)
  (list
   (+ (car a) (car b))
   (+ (cadr a) (cadr b))))

(defun v2- (a b)
  (list
   (- (car a) (car b))
   (- (cadr a) (cadr b))))

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

;; can be <1..8> * vec ("sliding")
(define-constant +rook-offsets+ '((1 0) (-1 0) (0 1) (0 -1)) :test #'equal)
(define-constant +bishop-offsets+ '((1 1) (1 -1) (-1 1) (-1 -1)) :test #'equal)
(define-constant +queen-offsets+ (append +rook-offsets+ +bishop-offsets+) :test #'equal)

(define-constant +king-moves+ '((1 0) (-1 0) (0 1) (0 -1) (-1 -1) (1 -1) (-1 1) (1 1)) :test #'equal)
(define-constant +knight-moves+ '((1 2) (1 -2) (-1 2) (-1 -2) (2 1) (-2 1) (2 -1) (-2 -1)) :test #'equal)
;; not defining pawn moves because they "depend" :3

(defun print-castle-rules (g)
  (declare (type game g))
  (format t "white: (Q: ~a, K: ~a), black: (Q: ~a, K: ~a)~%"
          (game-white-can-castle-queenside-p g)
          (game-white-can-castle-kingside-p g)
          (game-black-can-castle-queenside-p g)
          (game-black-can-castle-kingside-p g)))

;; e4 -> (4 4)
(defun pos->lst (s)
  (declare (type string s))
  (when (= (length s) 2)
    (list
     (- (char-int (aref s 0)) (char-int #\a))
     (- 8 (- (char-int (aref s 1)) (char-int #\0))))))

(defun fen->game (fen)
  (declare (type string fen))

  (let* ((l (split "\\s" fen))
         (fens (nth 0 l))
         (castle-rules (nth 2 l))
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
     :ticker (if (equal (nth 1 l) "w") 0 1)
     ;; TODO: ^ to nie jest prawda, ticker powinien sie zgadzać z fenem
     :move-history nil
     :wck-p (eq (aref castle-rules 0) #\K)
     :wcq-p (eq (aref castle-rules 1) #\Q)
     :bck-p (eq (aref castle-rules 2) #\k)
     :bcq-p (eq (aref castle-rules 3) #\q)
     :en-passant-target-square (pos->lst (nth 3 l))
     )))

(define-constant +initial-fen+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" :test #'equal)

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

(defun filter-own-pieces (game p move-list &key disallow-taking check-mode)
  (if check-mode
      move-list
      (remove-if
       #'(lambda (pos)
           (when-let ((p* (piece-at-point game (car pos) (cadr pos))))
             (or (eq (piece-color p*) (piece-color p)) disallow-taking)))
       move-list)))

;; i used 100% of my brain for this name
(defun enposition-moveset (position moveset)
  (mapcar #'(lambda (p) (v2+ p position)) moveset))

(defun generate-sliding-moves (game p moveset &key check-mode)
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
                ((and p* (eq (piece-color p) (piece-color p*))) ; same-colored piece, no more moves unless check mode, then defended
                 (when check-mode
                   (push `(,x* ,y*) acc))
                 (return-from brk))
                (p* ; diff-colored piece, can be captured but no more unless it's a king in check mode
                 (push `(,x* ,y*) acc)
                 (unless (and
                          check-mode
                          (when-let ((p (piece-at-point game x* y*)))
                            (eq (piece-type p) 'king)))
                   (return-from brk)))
                (t ; empty square, can go there and possibly further
                 (push `(,x* ,y*) acc)))))))
      acc)))

(defun maybe-castling-moves (game p)
  (declare (type game game)
           (type piece p))
  ;; TODO: checks!!!!
  (append
   ;; white
   (when (and
          (whitep p)
          (game-white-can-castle-kingside-p game)
          (null (piece-at-point game 5 7))
          (null (piece-at-point game 6 7))
          (eq (piece-type (piece-at-point game 7 7)) 'rook))
     (list
      (list 6 7 #'(lambda (game)
                    (let ((r (piece-at-point game 7 7)))
                      (setf (game-white-can-castle-kingside-p game) nil)
                      (setf (game-white-can-castle-queenside-p game) nil)
                      (setf (piece-point r) (make-instance 'point :x 5 :y 7)))))))
   (when (and
          (whitep p)
          (game-white-can-castle-queenside-p game)
          (null (piece-at-point game 1 7))
          (null (piece-at-point game 2 7))
          (null (piece-at-point game 3 7))
          (eq (piece-type (piece-at-point game 0 7)) 'rook))
     (list
      (list 2 7 #'(lambda (game)
                    (let ((r (piece-at-point game 0 7)))
                      (setf (game-white-can-castle-kingside-p game) nil)
                      (setf (game-white-can-castle-queenside-p game) nil)
                      (setf (piece-point r) (make-instance 'point :x 3 :y 7)))))))
   ;; black
   (when (and
          (blackp p)
          (game-black-can-castle-kingside-p game)
          (null (piece-at-point game 5 0))
          (null (piece-at-point game 6 0))
          (eq (piece-type (piece-at-point game 7 0)) 'rook))
     (list
      (list 6 0 #'(lambda (game)
                    (let ((r (piece-at-point game 7 0)))
                      (setf (game-black-can-castle-kingside-p game) nil)
                      (setf (game-black-can-castle-queenside-p game) nil)
                      (setf (piece-point r) (make-instance 'point :x 5 :y 0)))))))
   (when (and
          (blackp p)
          (game-black-can-castle-queenside-p game)
          (null (piece-at-point game 1 0))
          (null (piece-at-point game 2 0))
          (null (piece-at-point game 3 0))
          (eq (piece-type (piece-at-point game 0 0)) 'rook))
     (list
      (list 2 0 #'(lambda (game)
                    (let ((r (piece-at-point game 0 0)))
                      (setf (game-black-can-castle-kingside-p game) nil)
                      (setf (game-black-can-castle-queenside-p game) nil)
                      (setf (piece-point r) (make-instance 'point :x 3 :y 0)))))))
   ))

(defun pre--possible-moves-for (game p check-mode)
  (multiple-value-bind (x y)
      (position-of p)
    (case (piece-type p)
      ;; TODO: ale swietny kod
      ;; TODO: no troche spaghetti
      (pawn (let* ((m (when (not check-mode)
                        (filter-own-pieces game p (list (v2+ (list x y) (list 0 (if (whitep p) -1 1)))) :disallow-taking t)))
                   (m (when (not check-mode)
                        (if m
                            (append
                             m
                             (filter-own-pieces
                              game p
                              (append
                               (if (and (whitep p) (= (point-y (piece-point p)) 6))
                                   (let ((pos (v2+ (list x y) '(0 -2))))
                                     (list (append
                                            pos
                                            (list
                                             #'(lambda (game) (setf (game-en-passant-target-square game) pos))))))
                                   nil)
                               (if (and (blackp p) (= (point-y (piece-point p)) 1))
                                   (let ((pos (v2+ (list x y) '(0 2))))
                                     (list (append
                                            pos
                                            (list
                                             #'(lambda (game) (setf (game-en-passant-target-square game) pos))))))
                                   nil))
                              :disallow-taking t))
                            nil))))

              ;; going forwards done, generate capturing moves
              (macrolet ((maybe-pushmove (x* y*)
                           ;; en passant?
                           `(let* ((ts (game-en-passant-target-square game))
                                   (pt (when ts
                                         (if (whitep p)
                                             (v2- (game-en-passant-target-square game) '(0 1))
                                             (v2+ (game-en-passant-target-square game) '(0 1))))))
                              (if (and ts (equal (list ,x* ,y*) pt))
                                  (push
                                   (list ,x*
                                         ,y*
                                         #'(lambda (game)
                                             (let ((p (piece-at-point game (car ts) (cadr ts))))
                                               (setf (game-pieces game)
                                                     (remove p (game-pieces game) :test #'equal)))))
                                   m)
                                  ;; normal capturing
                                  (when-let ((p* (or check-mode (piece-at-point game ,x* ,y*))))
                                    (when (or check-mode (not (eq (piece-color p) (piece-color p*))))
                                      (push (list ,x* ,y*) m)))))))
                (if (whitep p)
                    (progn
                      (maybe-pushmove (- x 1) (- y 1))
                      (maybe-pushmove (+ x 1) (- y 1)))
                    (progn
                      (maybe-pushmove (- x 1) (+ y 1))
                      (maybe-pushmove (+ x 1) (+ y 1)))))
              m))
      (knight (filter-own-pieces game p (enposition-moveset (list x y) +knight-moves+) :check-mode check-mode))
      (king   (append
               (remove-if
                #'(lambda (pos)
                    (unless check-mode
                      (point-checked-p game (car pos) (cadr pos) (if (whitep p) 'black 'white))))
                (filter-own-pieces game p (enposition-moveset (list x y) +king-moves+)))
               ;; TODO: check for checks in maybe-castling-moves
               (maybe-castling-moves game p)))
      (rook   (generate-sliding-moves game p +rook-offsets+   :check-mode check-mode))
      (bishop (generate-sliding-moves game p +bishop-offsets+ :check-mode check-mode))
      (queen  (generate-sliding-moves game p +queen-offsets+  :check-mode check-mode))
      (t
       (warn "unreachable reached D:")))))

;; TODO: cache that
(defun king-of (game color)
  (block b
    (loop for p in (game-pieces game) do
      (when (and (eq (piece-type p) 'king)
                 (eq (piece-color p) color))
        (return-from b p)))
    nil))

(defun possible-moves-for (game p &key check-mode)
  (declare (type piece p)
           (type game game))
  (let ((point (piece-point p)))
    (remove-if
     #'(lambda (pos)
         (when (not check-mode)
           (let ((p-was (piece-at-point game (car pos) (cadr pos))))
             (setf (piece-point p) (make-instance 'point :x (car pos) :y (cadr pos)))
             (let* ((king (king-of game (piece-color p)))
                    (king-point (piece-point king)))
               (when p-was
                 (setf (game-pieces game) (remove p-was (game-pieces game) :test #'equal)))
               (prog1
                   (point-checked-p game (point-x king-point) (point-y king-point) (if (whitep king) 'black 'white))
                 (setf (piece-point p) point)
                 (when p-was
                   (push p-was (game-pieces game))))))))
     (remove-if
      #'(lambda (pos)
          (or
           (< (car pos) 0)
           (< (cadr pos) 0)
           (> (car pos) 7)
           (> (cadr pos) 7)))
      (pre--possible-moves-for game p check-mode)))))

(defun move-possible-p (p px py game &key check-mode)
  (block b
    (loop for m in (possible-moves-for game p :check-mode check-mode) do
      (when (and (eq px (car m)) (eq py (cadr m)))
        (return-from b (if (caddr m) (caddr m) t))))
    nil))

;; by: checked by ('white or 'black)
(defun point-checked-p (game px py by)
  (declare (type game game)
           (type number px)
           (type number py)
           (type symbol by))
  (assert (or
           (eq by 'white)
           (eq by 'black)))
  (let ((checkers (remove-if
                   #'(lambda (p) (not (eq by (piece-color p))))
                   (game-pieces game))))
    (block brk
      (loop for p in checkers do
        (when (move-possible-p p px py game :check-mode t)
          (format t "point (~a, ~a) is checkable by ~a (~a)~%" px py p (piece-type p))
          (return-from brk t)))
      nil)))

(defparameter maybe-drag/piece nil)
;; woah
;; so idk how the (functionp (caddr thing)) will work in the future lol !
(defun maybe-drag (game)
  (declare (type game game))
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (cond
      ((mouse-pressed-p 0)  ; begin dragging
       (when-let ((p (piece-at-point game px py)))
         (when (eq (piece-color p) (game-turn game))
           (setf maybe-drag/piece p))))
      ((and (mouse-released-p 0) maybe-drag/piece); end dragging
       (when-let ((f (move-possible-p maybe-drag/piece px py game)))
         (when-let ((p (piece-at-point game px py)))
           (setf (game-pieces game)
                 (remove p (game-pieces game) :test #'equal)))

         (when (and
                (game-white-can-castle-kingside-p game)
                (eq (piece-type maybe-drag/piece) 'rook)
                (= (point-x (piece-point maybe-drag/piece)) 7)
                (= (point-y (piece-point maybe-drag/piece)) 7))
           (setf (game-white-can-castle-kingside-p game) nil))

         (when (and
                (game-white-can-castle-queenside-p game)
                (eq (piece-type maybe-drag/piece) 'rook)
                (= (point-x (piece-point maybe-drag/piece)) 0)
                (= (point-y (piece-point maybe-drag/piece)) 7))
           (setf (game-white-can-castle-queenside-p game) nil))

         (when (and
                (game-black-can-castle-kingside-p game)
                (eq (piece-type maybe-drag/piece) 'rook)
                (= (point-x (piece-point maybe-drag/piece)) 7)
                (= (point-y (piece-point maybe-drag/piece)) 0))
           (setf (game-black-can-castle-kingside-p game) nil))

         (when (and
                (game-black-can-castle-queenside-p game)
                (eq (piece-type maybe-drag/piece) 'rook)
                (= (point-x (piece-point maybe-drag/piece)) 0)
                (= (point-y (piece-point maybe-drag/piece)) 0))
           (setf (game-black-can-castle-queenside-p game) nil))

         ;; (format t "en-passant-target-square was ~a~%" (game-en-passant-target-square game))
         (setf (game-en-passant-target-square game) nil)

         ; move thing
         (setf (piece-point maybe-drag/piece)
               (make-instance 'point :x px :y py))
         (game-tick game)

         (when (eq (piece-type maybe-drag/piece) 'king)
           (if (blackp maybe-drag/piece)
               (progn
                 (setf (game-black-can-castle-kingside-p game) nil)
                 (setf (game-black-can-castle-queenside-p game) nil))
               (progn
                 (setf (game-white-can-castle-kingside-p game) nil)
                 (setf (game-white-can-castle-queenside-p game) nil))))

         (when (functionp f) ;; TODO: this is a freaky hack
           ;; this funcall can:
           ;;  * move rook after castling
           ;;  * set en-passant-target-square
           ;;  * delete a pawn after en passant
           (funcall f game)))

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

(defun describe-checked (game)
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (draw-text
     (if (point-checked-p game px py 'black)
         "yes"
         "no")
     0
     100
     32
     +color-white+)))
(add-draw-hook 'describe-checked)

(add-draw-hook 'show-point-at-cursor)
(add-draw-hook 'maybe-drag)
(add-draw-hook 'highlight-possible-moves)

(defun load-textures ()
  (setf white-texture-alist nil)
  (setf black-texture-alist nil)
  (macrolet ((load* (data-list alist)
               `(dolist (e ,data-list)
                  (let ((texture (make-texture (cdr e) ".png")))
                    ;; TODO: czemu tekstury są tak rozpikselizowane lol
                    (set-texture-filter! texture +TEXTURE-FILTER-POINT+)
                    (push (cons (car e) texture) ,alist)))))

    (load* white-texture-data-list white-texture-alist)
    (load* black-texture-data-list black-texture-alist)

    (format t "loaded textures~%")))

;; (define-constant +test-fen+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1" :test #'equal)

(defun main (&optional argv)
  (declare (ignore argv))

  (init-window (* +piece-size+ 8) (* +piece-size+ 8) "hello")
  (set-target-fps! 50)
  (set-exit-key! -1)

  (load-textures)

  (format t "white-texture-alist: ~a~%" white-texture-alist)
  (format t "black-texture-alist: ~a~%" black-texture-alist)

  (let ((game (fen->game +initial-fen+)))

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
