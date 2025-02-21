;;; Leszcz entrypoint

(defpackage :leszcz
  (:use :common-lisp :leszcz-constants :leszcz-types :raylib :gui :alexandria :cl-ppcre :net :fast :cl-mop :cffi)
  (:export
   main))

(in-package :leszcz)

(defvar *threads* nil)

;; do not use this, this is only for eval in repl
(defparameter *current-game* nil)
(defparameter *current-screen* nil)

(defparameter *current-board-evaluation* nil)

(defun hasp (el l)
  (member el l :test #'equal))

(defmethod game-turn ((g game))
  (if (= (mod (the fixnum (game-ticker g)) 2) 0) 'white 'black))

(defmethod game-turn-white-p ((g game))
  (eq (game-turn g) 'white))

(defmethod game-turn-black-p ((g game))
  (eq (game-turn g) 'black))

;; assuming a "vector" or "vector2" is a (list a b)
(defun v2+ (a b)
  (list
   (+ (car a) (car b))
   (+ (cadr a) (cadr b))))

(defun v2- (a b)
  (declare (type list a b))
  (list
   (- (car a) (car b))
   (- (cadr a) (cadr b))))

(defmacro maybe-reverse (g pt)
  `(if (eq (game-side ,g) 'white)
       ,pt
       (- 7 ,pt)))

(defparameter draw-piece/anim-frame-ticker 0)
(defparameter draw-piece/anim-frame 0)
(defparameter draw-piece/piece-on-point nil)

(defun draw-piece (g p)
  (declare (type piece p)
           (type game g))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) ;; meh this is not that important

  (let* ((point (piece-point p))
         (x (* +piece-size+ (maybe-reverse g (point-x point))))
         (y (* +piece-size+ (maybe-reverse g (point-y point))))
         (al (if (whitep p) white-texture-alist black-texture-alist))
         (texture (cdr (assoc (piece-type p) al)))
         (bx (car *board-begin*))
         (by (cdr *board-begin*)))

    (multiple-value-bind (px py)
        (coords->point (mouse-x) (mouse-y))
      (let ((px (maybe-reverse g px))
            (py (maybe-reverse g py)))
        (cond
          ((and (vectorp texture) (= (point-x point) px) (= (point-y point) py) (eq draw-piece/piece-on-point p))
           ;; Drawing piece at current mouse point and we have done that before
           (incf draw-piece/anim-frame-ticker)
           (when (= 0 (mod draw-piece/anim-frame-ticker 15))
             (incf draw-piece/anim-frame))
           (setf texture (aref texture (mod draw-piece/anim-frame (length texture)))))
          ((and (vectorp texture) (= (point-x point) px) (= (point-y point) py))
           ;; Drawing piece at current mouse point and we have NOT done that before
           (setf draw-piece/anim-frame-ticker 0)
           (setf draw-piece/anim-frame 0)
           (setf draw-piece/piece-on-point p)
           (setf texture (aref texture 0))
           )
          ((vectorp texture)
           (setf texture (aref texture 0))))

        (draw-texture
         texture
         (floatize (list 0 0 +texture-size+ +texture-size+))
         (floatize (list (+ bx x) (+ by y) +piece-size+ +piece-size+))
         (floatize (list 0 0))
         (float 0)
         +color-white+)))))

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

;; (4 4) -> e4
(defun lst->pos (l)
  (declare (type list l))
  (coerce
   (list
    (code-char (+ (car l) (char-int #\a)))
    (code-char (+ (- 8 (cadr l)) (char-int #\0))))
   'string))

(defun piece->char (p)
  (declare (type piece p)
           (values character))
  (let ((c (case (piece-type p)
             (king   #\k)
             (queen  #\q)
             (rook   #\r)
             (bishop #\b)
             (knight #\n)
             (pawn   #\p))))
    (if (whitep p) (char-upcase c) c)))

(defun game->fen (g)
  (declare (type game g))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (let ((result "")
        (acc 0))
    (loop for y from 0 to 7 do
      (loop for x from 0 to 7 do
        (if-let ((p (piece-at-point g x y)))
          (progn
            (when (> (the fixnum acc) 0)
              (setf result (concatenate 'string result (write-to-string acc))))
            (setf acc 0)
            (setf result (concatenate 'string result (string (piece->char p)))))
          (incf (the fixnum acc))))
      (when (> (the fixnum acc) 0)
        (setf result (concatenate 'string result (write-to-string acc))))
      (setf acc 0)
      (when (< y 7)
        (setf result (concatenate 'string result "/"))))
    (setf result (concatenate 'string result (if (game-turn-white-p g) " w " " b ")))
    (if (or (game-white-can-castle-kingside-p g)
            (game-white-can-castle-queenside-p g)
            (game-black-can-castle-kingside-p g)
            (game-black-can-castle-queenside-p g))
        (progn
          (when (game-white-can-castle-kingside-p g)
            (setf result (concatenate 'string result "K")))
          (when (game-white-can-castle-queenside-p g)
            (setf result (concatenate 'string result "Q")))
          (when (game-black-can-castle-kingside-p g)
            (setf result (concatenate 'string result "k")))
          (when (game-black-can-castle-queenside-p g)
            (setf result (concatenate 'string result "q"))))
        (setf result (concatenate 'string result "-")))
    (if-let ((s (game-en-passant-target-square g)))
      (setf result (concatenate 'string result " " (lst->pos s) " "))
      (setf result (concatenate 'string result " - ")))
    (setf result (concatenate 'string result (write-to-string (game-halfmove-clock g)) " " (write-to-string (game-fullmove-clock g))))))

(defun fen->game (fen)
  (declare (type string fen))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

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
     :ticker (if (equal (nth 1 l) "w") 0 1) ;; ticker jest adhoc
     :halfmove-clock (parse-integer (nth 4 l))
     :fullmove-clock (parse-integer (nth 5 l))
     :move-history nil
     :wck-p (find #\K (the string castle-rules))
     :wcq-p (find #\Q (the string castle-rules))
     :bck-p (find #\k (the string castle-rules))
     :bcq-p (find #\q (the string castle-rules))
     :en-passant-target-square (pos->lst (nth 3 l))
     :side 'white
     :connection nil
     )))

(defun draw-game (g)
  (declare (type game g))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((i (if (eq (game-side g) 'white) 0 1))
        (bx (car *board-begin*))
        (by (cdr *board-begin*)))
    (loop for y from 0 to 7 do
      (loop for x from 0 to 7 do
        (let ((color (if (= (mod (+ y x) 2) 0) *color-bg-light* *color-bg-dark*)))
          (draw-rectangle
           (+ bx (* +piece-size+ x))
           (+ by (* +piece-size+ y))
           +piece-size+ +piece-size+ color)
          (incf i)))))

  (let ((pieces (game-pieces g)))
    (dolist (p pieces)
      (draw-piece g p))))

(defun coords->point (x y)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((x* (- x (car *board-begin*)))
        (y* (- y (cdr *board-begin*))))
    (values
     (floor (/ x* +piece-size+))
     (floor (/ y* +piece-size+)))))

(defun position-of (p)
  (declare (type piece p))
  (values
   (point-x (piece-point p))
   (point-y (piece-point p))))

(defun show-point-at-cursor (g &rest r)
  (declare (ignore r))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (draw-rectangle-lines
     (+ (car *board-begin*) (* px +piece-size+))
     (+ (cdr *board-begin*) (* py +piece-size+))
     +piece-size+ +piece-size+ +color-black+)
    (when *debug*
      (let ((checked-by 'white))
        (when-let ((d (new--point-checked-p g px py checked-by)))
          (format t "checked by: ~a~%" d)
          (draw-rectangle
           (+ (car *board-begin*) (* px +piece-size+))
           (+ (cdr *board-begin*) (* py +piece-size+))
           +piece-size+ +piece-size+ +color-redish+))))))

(deftype place ()
  '(integer -15 15))

(defun piece-at-point (game x y)
  (when (and (>= x 0) (< x 8) (>= y 0) (< y 8))
    (if-let ((a (game-points-cache game)))
      (aref a (+ x (* y 8)))
      (let ((pieces (game-pieces game)))
        (dolist (p pieces)
          (when (and (= (the fixnum (point-x (piece-point p))) x)
                     (= (the fixnum (point-y (piece-point p))) y))
            (return-from piece-at-point p)))
        nil))))

(defun filter-own-pieces (game p move-list &key disallow-taking)
  (remove-if
   #'(lambda (pos)
       (when-let ((p* (piece-at-point game (car pos) (cadr pos))))
         (or (eq (piece-color p*) (piece-color p)) disallow-taking)))
   move-list))

;; i used 100% of my brain for this name
(defun enposition-moveset (position moveset)
  (declare (optimize (speed 3) (safety 0)))
  (loop for m in moveset collect (v2+ m position)))

(defun generate-sliding-moves (game p moveset)
  (declare (type game game)
           (type piece p))
  (declare (optimize (speed 3) (safety 0)))

  (multiple-value-bind (x y)
      (position-of p)
    (let ((acc nil))
      (loop for m in moveset do
        (block brk
          (loop for i from 1 to 8 do
            (let* ((x* (+ (the place x) (* (the fixnum i) (the place (car m)))))
                   (y* (+ (the place y) (* (the fixnum i) (the place (cadr m)))))
                   (p* (piece-at-point game x* y*)))
              (cond
                ((< x* 0)  (return-from brk))
                ((< y* 0)  (return-from brk))
                ((>= x* 8) (return-from brk))
                ((>= y* 8) (return-from brk))
                ((and p* (eq (piece-color p) (piece-color p*))) ; same-colored piece, no more moves unless check mode, then defended
                 (return-from brk))
                (p* ; diff-colored piece, can be captured but no more unless it's a king in check mode
                 (push `(,x* ,y*) acc)
                 (unless (when-let ((p (piece-at-point game x* y*)))
                           (eq (piece-type p) 'king))
                   (return-from brk)))
                (t ; empty square, can go there and possibly further
                 (push `(,x* ,y*) acc)))))))
      acc)))

(defmacro safe-piece-type (p)
  `(if-let ((v ,p))
     (piece-type v)
     nil))

(defun maybe-castling-moves (game p)
  (declare (type game game)
           (type piece p))
  (append
   ;; white
   (when (and
          (whitep p)
          (game-white-can-castle-kingside-p game)
          (null (piece-at-point game 5 7))
          (null (piece-at-point game 6 7))
          (not (point-checked-p game 5 7 'black))
          (not (point-checked-p game 6 7 'black))
          (eq (safe-piece-type (piece-at-point game 7 7)) 'rook))
     (list
      (list 6 7 #'(lambda (g*)
                    (let ((r (piece-at-point g* 7 7)))
                      (setf (game-white-can-castle-kingside-p g*) nil)
                      (setf (game-white-can-castle-queenside-p g*) nil)
                      (setf (piece-point r) (make-instance 'point :x 5 :y 7)))))))
   (when (and
          (whitep p)
          (game-white-can-castle-queenside-p game)
          (null (piece-at-point game 1 7))
          (null (piece-at-point game 2 7))
          (null (piece-at-point game 3 7))
          (not (point-checked-p game 2 7 'black))
          (not (point-checked-p game 3 7 'black))
          (eq (safe-piece-type (piece-at-point game 0 7)) 'rook))
     (list
      (list 2 7 #'(lambda (g*)
                    (let ((r (piece-at-point g* 0 7))) ;; TY KURWO JEBANA KRZYSZTOF CZEMU TU BYŁO PIECE-AT-POINT GAME ??!?!?
                      (setf (game-white-can-castle-kingside-p g*) nil)
                      (setf (game-white-can-castle-queenside-p g*) nil)
                      (setf (piece-point r) (make-instance 'point :x 3 :y 7)))))))
   ;; black
   (when (and
          (blackp p)
          (game-black-can-castle-kingside-p game)
          (null (piece-at-point game 5 0))
          (null (piece-at-point game 6 0))
          (not (point-checked-p game 5 0 'white))
          (not (point-checked-p game 6 0 'white))
          (eq (safe-piece-type (piece-at-point game 7 0)) 'rook))
     (list
      (list 6 0 #'(lambda (g*)
                    (let ((r (piece-at-point g* 7 0)))
                      (setf (game-black-can-castle-kingside-p g*) nil)
                      (setf (game-black-can-castle-queenside-p g*) nil)
                      (setf (piece-point r) (make-instance 'point :x 5 :y 0)))))))
   (when (and
          (blackp p)
          (game-black-can-castle-queenside-p game)
          (null (piece-at-point game 1 0))
          (null (piece-at-point game 2 0))
          (null (piece-at-point game 3 0))
          (not (point-checked-p game 2 0 'white))
          (not (point-checked-p game 3 0 'white))
          (eq (safe-piece-type (piece-at-point game 0 0)) 'rook))
     (list
      (list 2 0 #'(lambda (g*)
                    (let ((r (piece-at-point g* 0 0)))
                      (setf (game-black-can-castle-kingside-p g*) nil)
                      (setf (game-black-can-castle-queenside-p g*) nil)
                      (setf (piece-point r) (make-instance 'point :x 3 :y 0)))))))
   ))

;; It's okay to use *current-game* here as this function will be called only when running interactively
(defun ut/make-texture-lambda (type)
  #'(lambda () (let ((al (if (eq (game-side *current-game*) 'white)
                        white-texture-alist
                        black-texture-alist)))
            (base-texture-of (cdr (assoc type al))))))

(defparameter ut/upgrade-queen-texture  (ut/make-texture-lambda 'queen))
(defparameter ut/upgrade-rook-texture   (ut/make-texture-lambda 'rook))
(defparameter ut/upgrade-knight-texture (ut/make-texture-lambda 'knight))
(defparameter ut/upgrade-bishop-texture (ut/make-texture-lambda 'bishop))

(defparameter ut/upgrade-size +piece-size+)
(defparameter ut/pad-size (/ +piece-size+ 2))

(defun ask-for-upgrade-type (game upgraded-piece)
  (declare (type game game)
           (type piece upgraded-piece))
  (declare (ignore game))
  (macrolet ((shid (texture ident)
               `(make-button* ,texture :height ut/upgrade-size :width ut/upgrade-size :background-color (if (whitep upgraded-piece) +color-white+ +color-black+) :identifier ,ident)))
    (let* ((bq (shid ut/upgrade-queen-texture  'queen))
           (br (shid ut/upgrade-rook-texture   'rook))
           (bn (shid ut/upgrade-knight-texture 'knight))
           (bb (shid ut/upgrade-bishop-texture 'bishop))
           (bx (car *board-begin*))
           (by (cdr *board-begin*)))
      ;; Okay so basically i can't do that asynchronously (yet?) as the mainloop won't know that the turn did not end yet
      ;; So this is a big fucking TODO and a HACK im just stealing the mainloop here look
      (let* ((clicked nil)
             (cleanup #'(lambda (ident)
                          (setf clicked ident)))
             (bg (image->texture *current-screen*)))
        (end-drawing)
        (loop while (not clicked) do
          (begin-drawing)
          (clear-background +color-white+)

          ;; this is fucked up --------------+
          (draw-texture                 ;    |
           bg                           ; <--+
           (floatize (list 0 0 *window-width* *window-height*))
           (floatize (list 0 0 *window-width* *window-height*))
           (floatize '(0 0)) (float 0) +color-white+)

          (funcall bq (+ bx ut/upgrade-size) (+ by ut/upgrade-size) cleanup)
          (funcall br (+ bx (* 2 ut/upgrade-size) ut/pad-size)       (+ by ut/upgrade-size) cleanup)
          (funcall bn (+ bx (* 3 ut/upgrade-size) (* 2 ut/pad-size)) (+ by ut/upgrade-size) cleanup)
          (funcall bb (+ bx (* 4 ut/upgrade-size) (* 3 ut/pad-size)) (+ by ut/upgrade-size) cleanup)
          (set-mouse-cursor! +cursor-normal+)
          (end-drawing))
        (begin-drawing)
        (unload-texture! bg)
        clicked))))

(defun pre--possible-moves-for/upgrade (game p next-pos)
  (declare (type piece p))
  (if (and
       (eq (piece-type p) 'pawn)
       (or (= 0 (the place (cadr next-pos)))
           (= 7 (the place (cadr next-pos)))))
      (let ((px (point-x (piece-point p)))
            (py (point-y (piece-point p))))
        (flet ((f (type)
                 #'(lambda (g*)
                     (if (game-interactive-p g*)
                         (progn
                           (format t "asking interactively for upgrade type!~%")
                           (setf (piece-type p) (ask-for-upgrade-type g* p)))
                         (let ((p (piece-at-point g* px py)))
                           (setf (piece-type p) type))))))
          (list
           (append next-pos `(,(f 'queen)))
           (append next-pos `(,(f 'rook)))
           (append next-pos `(,(f 'bishop)))
           (append next-pos `(,(f 'knight))))))
      `(,next-pos)))

;; (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'queen)))))
;; (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'rook)))))
;; (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'bishop)))))
;; (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'knight))))))))

      ;; (if (game-interactive-p game)
      ;;     (list
      ;;      (append next-pos (list #'(lambda (g*) ;; Ask interactively for upgrade type, also because asking interactively, just use p instead of looking for piece-at-point
      ;;       (list ;; Generate all possible updgrade options and let "le computer" choose

      ;; (list next-pos)))

(defun pre--possible-moves-for/pawn (game p)
  (declare (type game game)
           (type piece p)
           (values list))
  (let-values ((x y (position-of p)))
    (let* ((m (filter-own-pieces
               game p
               (pre--possible-moves-for/upgrade game p (v2+ (list x y) (list 0 (if (whitep p) -1 1))))
               :disallow-taking t))
           (m (if m
                  (append
                   m
                   (filter-own-pieces
                    game p
                    (append
                     (if (and (whitep p) (= (the place (point-y (piece-point p))) 6))
                         (let ((pos (v2+ (list x y) '(0 -2))))
                           (list (append
                                  pos
                                  (list
                                   #'(lambda (g*)
                                       (setf (game-en-passant-target-square g*) pos))))))
                         nil)
                     (if (and (blackp p) (= (the place (point-y (piece-point p))) 1))
                         (let ((pos (v2+ (list x y) '(0 2))))
                           (list (append
                                  pos
                                  (list
                                   #'(lambda (g*)
                                       (setf (game-en-passant-target-square g*) pos))))))
                         nil))
                    :disallow-taking t))
                  nil)))

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
                                 #'(lambda (g*)
                                     (let ((p (piece-at-point g* (car ts) (cadr ts))))
                                       (setf (game-pieces g*)
                                             (remove p (game-pieces g*) :test #'equal)))))
                           m)
                          ;; normal capturing
                          (when-let ((p* (piece-at-point game ,x* ,y*)))
                            (when (not (eq (piece-color p) (piece-color p*)))
                              (let ((vs* (pre--possible-moves-for/upgrade game p (list ,x* ,y*))))
                                (dolist (v vs*)
                                  (push v m)))))))))
        (if (whitep p)
            (progn
              (maybe-pushmove (- x 1) (- y 1))
              (maybe-pushmove (+ x 1) (- y 1)))
            (progn
              (maybe-pushmove (- x 1) (+ y 1))
              (maybe-pushmove (+ x 1) (+ y 1)))))
      m)))

;; (defun old--pre--possible-moves-for (game p)
;;   (multiple-value-bind (x y)
;;       (position-of p)
;;     (case (piece-type p)
;;       ;; TODO: ale swietny kod
;;       ;; TODO: no troche spaghetti
;;       ;; TODO: ja pierdole
;;       ;; TODO: zastrzele sie
;;       (pawn   (pre--possible-moves-for/pawn game p))
;;       (knight (filter-own-pieces game p (enposition-moveset (list x y) +knight-moves+)))
;;       (king   (append
;;                (remove-if
;;                 #'(lambda (pos)
;;                     (point-checked-p game (car pos) (cadr pos) (if (whitep p) 'black 'white)))
;;                 (filter-own-pieces game p (enposition-moveset (list x y) +king-moves+)))
;;                ;; TODO: check for checks in maybe-castling-moves
;;                (maybe-castling-moves game p)))
;;       (rook   (generate-sliding-moves game p +rook-offsets+))
;;       (bishop (generate-sliding-moves game p +bishop-offsets+))
;;       (queen  (generate-sliding-moves game p +queen-offsets+))
;;       (t
;;        (warn "unreachable reached D:")))))

(defun bb->move-lst (bb)
  (declare (type (unsigned-byte 64) bb)
           (values list))
  (let ((l nil))
    (loop for y from 0 below 8 do
      (loop for x from 0 below 8 do
        (when (fast:bit-set-p bb (+ x (* y 8)) :type-size 64)
          (push `(,x ,y) l))))
    l))

(defmacro for-every-bb (as n &body b)
  ;; n to tak naprawdę fb tylko dużo zabawniejszy jest let pacan
  (append                                        ;          |
   '(progn)                                      ;          |
   (apply                                        ;          |
    #'append                                     ;          |
    (loop                                        ;          |
      for ca in '(fb-white fb-black)             ;          |
      collect (loop for pa in '(fb-pawn fb-rook fb-knight fb-bishop fb-queen fb-king)
                    collect                      ;          |
                    `(let ((,as (,pa (,ca ,n)))) ; <- tu o -+
                       ,@b                       ;
                       (setf (,pa (,ca ,n)) ,as) ; a tu to nawet pacanas!
                       ))))))

;;; not for king and pawn as they require additional funcalls
;; bb: a bitboard of moves to check
;; from: a bitboard of the piece location
;; color-accessor: fb-white or fb-black
;; piece-accessor: same as above but for pieces
(defmacro fb-filter-check-moves (fb* color-accessor piece-accessor from bb)
  `(progn
     (let-values ((bb ,bb) ;; make sure it's in a variable
                  (fb ,fb*)
                  (kx ky (fb1-king-of (,color-accessor fb))))
       (setf (,piece-accessor (,color-accessor fb))
             (logxor (,piece-accessor (,color-accessor fb)) ,from)) 
       (loop for i from 0 below 64 do
         (when-let* ((_ (fast:bit-set-p bb i))
                     (m (ash 1 (- 63 i)))) ;; current move "bitboard"
           (let ((was (make-array '(12)))
                 (i* 0))

             (for-every-bb bitb fb
               (setf (aref was i*) bitb)
               (incf i*)
               (setf bitb (logand bitb (lognot64 m))))

             (setf (,piece-accessor (,color-accessor fb)) ;; make move (add m to fb)
                   (logior (,piece-accessor (,color-accessor fb)) m))

             (when-let ((by (fb-point-checked-p fb kx ky (if (eq ',color-accessor 'fb-white) 'black 'white))))
               ;; (format t "point map after move is: (would be checked by ~a)~%" by)
               ;; (fast::fb-display (fast::fb-make-piece-board fb))
               ;; (format t "check map of black is: ~%")
               ;; (fast::fb-display (fb-make-check-board fb 'black))
               (setf bb (logxor bb m)))

             (setf i* 0)
             (setf (,piece-accessor (,color-accessor fb)) ;; unmake move (delete new point = m)
                   (logxor (,piece-accessor (,color-accessor fb)) m))
             (for-every-bb bitb fb
               (setf bitb (aref was i*))
               (incf i*)))))
       (setf (,piece-accessor (,color-accessor fb)) ;; set piece back to point
             (logior (,piece-accessor (,color-accessor fb)) ,from))
       bb)))

      ;; (for-every-bb ,bb fb
      ;;   (setf bb (logior bb m)

(defmacro fb-filter-check-moves* (fb color piece-accessor x y bb)
  `(if (eq color 'white)
       (fb-filter-check-moves ,fb fb-white ,piece-accessor (ash 1 (- 63 (+ ,x (* 8 ,y)))) ,bb)
       (fb-filter-check-moves ,fb fb-black ,piece-accessor (ash 1 (- 63 (+ ,x (* 8 ,y)))) ,bb)))

(defun pre--possible-moves-for (game p)
  (declare (type game game)
           (type piece p)
           (values list))
  (let-values ((x y (position-of p))
               (fb (game-fb game))
               (color (piece-color p)))
    (case (piece-type p)
      (knight (bb->move-lst
               (fb-filter-check-moves* fb color fb-knight x y (fb-generate-knight-moves fb x y color))))
      (bishop (bb->move-lst
               (fb-filter-check-moves* fb color fb-bishop x y (fb-generate-bishop-moves fb x y color))))
      (queen  (bb->move-lst
               (fb-filter-check-moves* fb color fb-queen x y (fb-generate-queen-moves fb x y color))))
      (rook   (bb->move-lst
               (fb-filter-check-moves* fb color fb-rook x y (fb-generate-rook-moves fb x y color))))

      (pawn   (slow-filter-check-moves game p (pre--possible-moves-for/pawn game p)))
      (king   (slow-filter-check-moves game p (append
                                               (bb->move-lst (fb-generate-king-moves (game-fb game) x y (piece-color p)))
                                               (maybe-castling-moves game p))))
      (t
       (error "unknown piece-type: ~a" (piece-type p))))))

(defun slow-filter-check-moves (game p lst)
  (declare (type game game)
           (type piece p)
           (type list lst)
           (values list))

  (let ((point (piece-point p)))
    (remove-if
     #'(lambda (pos)
         ;; TODO: this reimplements game-do-move (badly) -- make it so i can just call stripped game-do-move
         (when (or (eq (piece-type p) 'king) (eq (piece-type p) 'pawn))
           (let ((g* (copy-game game)))
             ;; (setf (game-points-cache g*) nil) ;; <- dupa 2
             (setf (game-interactive-p g*) nil)
             (game-update-points-cache g*)

             ;; (let ((p-was (piece-at-point game (car pos) (cadr pos))))
             ;; (setf (piece-point p) (make-instance 'point :x (car pos) :y (cadr pos)))
             (let* ((king (king-of g* (piece-color p)))
                    (king-point (piece-point king))
                    (pt (piece-at-point g* (point-x point) (point-y point))))
               (when-let ((w (piece-at-point g* (car pos) (cadr pos))))
                 (setf (game-pieces g*) (remove w (game-pieces g*))))
               (setf (point-x (piece-point pt)) (car pos))
               (setf (point-y (piece-point pt)) (cadr pos))
               ;; this fucking sucks ↓
               (when (functionp (caddr pos))
                 (unless (and (eq (piece-type pt) 'pawn) (or (= (point-y point) 0) (= (point-y point) 7))) ; skip pawns so the user doesn't get FUCKING asked interactively during a move search...
                   (funcall (caddr pos) g*)))

               (game-update-points-cache g*) ;; TODO: only update the 2 things that might have changed
               (setf (game-fb g*) (game->fast-board g*)) ;; update fb, maybe -||-
               (point-checked-p
                g*
                (point-x king-point) (point-y king-point)
                (if (whitep king) 'black 'white))))))
     (remove-if
      #'(lambda (pos)
          (or
           (< (car pos) 0)
           (< (cadr pos) 0)
           (> (car pos) 7)
           (> (cadr pos) 7)))
      lst))))

(defun possible-moves-for (game p &key recache)
  (declare (type piece p)
           (type game game))
  (if recache
      (pre--possible-moves-for game p)
      (let ((p (piece-point p)))
        (cdr (assoc (list (point-x p) (point-y p)) (game-possible-moves-cache game) :test #'equal)))))

;; TODO: cache that
(defun king-of (game color)
  (block b
    (loop for p in (game-pieces game) do
      (when (and (eq (piece-type p) 'king)
                 (eq (piece-color p) color))
        (return-from b p)))
    (error "couldn't find a king of ~a in game ~a!" color game (game-move-history game))))

(defun display-mate (game)
  (declare (type game game))
  (format t "Mate! ~a won!~%" (if (eq (game-turn game) 'white) 'black 'white)))

(defun display-draw (game &optional why)
  (declare (type game game)
           (ignore game))
  (format t "Draw! (~a)~%" why))

;; game-possible-moves-cache: ((x y ((x' y') ...)))
(defmethod game-update-possible-moves-cache ((g game))
  (setf (game-possible-moves-cache g) nil)
  (dolist (p (game-pieces g))
    (when (eq (piece-color p) (game-turn g))
      (when-let ((possible (possible-moves-for g p :recache t)))
        (setf (game-possible-moves-cache g)
              (acons (list (point-x (piece-point p))
                           (point-y (piece-point p)))
                     possible
                     (game-possible-moves-cache g))))))

  ;; (format t "cache is ~a~%" (game-possible-moves-cache g))
  (values))

(defmethod game-update-points-cache ((g game))
  (let ((a (make-array '(64) :initial-element nil)))
    (dolist (p (game-pieces g))
      (let ((point (piece-point p)))
        (setf (aref a (+ (point-x point) (* 8 (point-y point)))) p)))
    (setf (game-points-cache g) a)))

(defmethod game-check-for-mates ((g game) &key (call-display t))
  (let ((c (game-possible-moves-cache g))
        (k (king-of g (game-turn g))))
    (cond
      ((and (null c) (point-checked-p g (point-x (piece-point k)) (point-y (piece-point k))
                                      (if (whitep k) 'black 'white)))
       ;; mate
       (setf (game-result g) 'checkmate)
       (when call-display
         (display-mate g)))
      ((null c)
       ;; stalemate
       (setf (game-result g) 'draw)
       (when call-display
         (display-draw g "stalemate")))
      ((>= (game-halfmove-clock g) 50)
       (setf (game-result g) 'draw)
       (when call-display
         (display-draw g "by 50 move rule")))
      (t
       (values)))))

(defun move-possible-p (p px py game)
  (block b
    (loop for m in (possible-moves-for game p) do
      (when (and (eq px (car m)) (eq py (cadr m)))
        (return-from b (if (caddr m) (caddr m) t))))
    nil))

;; by: checked by ('white or 'black)
;; TODO: macro-ize? manually unroll? look at disassembly?
(defun old--point-checked-p (game px py by)
  (or
   (fast::bit-set-p
    (fast::fb-make-check-board (game-fb game) by)
    (+ px (* 8 py)))
   ;; ←
   (block brk
     (loop for x from (- px 1) downto 0 by 1 do
       (when-let* ((p (piece-at-point game x py))
                   (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
         (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'rook)))
             (return-from brk 'rook1)
             (return-from brk nil)))))
   ;; →
   (block brk
     (loop for x from (+ px 1) below 8 do
       (when-let* ((p (piece-at-point game x py))
                  (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
         (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'rook)))
             (return-from brk 'rook2)
             (return-from brk nil)))))
   ;; ↑
   (block brk
     (loop for y from (- py 1) downto 0 by 1 do
       (when-let* ((p (piece-at-point game px y))
                  (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
         (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'rook)))
             (return-from brk 'rook3)
             (return-from brk nil)))))
   ;; ↓
   (block brk
     (loop for y from (+ py 1) below 8 do
       (when-let* ((p (piece-at-point game px y))
                  (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
         (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'rook)))
             (return-from brk 'rook4)
             (return-from brk nil)))))
   ;; →↓
   (block brk
     (loop for y from (+ py 1) below 8
           for x from (+ px 1) below 8
           do
              (when-let* ((p (piece-at-point game x y))
                         (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
                (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'bishop)))
                    (return-from brk 'bishop1)
                    (return-from brk nil)))))
   ;; →↑
   (block brk
     (loop for y from (- py 1) downto 0 by 1
           for x from (+ px 1) below 8
           do
              (when-let* ((p (piece-at-point game x y))
                         (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
                (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'bishop)))
                    (return-from brk 'bishop2)
                    (return-from brk nil)))))
   ;; ←↓
   (block brk
     (loop for y from (+ py 1) below 8
           for x from (- px 1) downto 0 by 1
           do
              (when-let* ((p (piece-at-point game x y))
                         (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
                (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'bishop)))
                    (return-from brk 'bishop3)
                    (return-from brk nil)))))
   ;; ←↑
   (block brk
     (loop for y from (- py 1) downto 0 by 1
           for x from (- px 1) downto 0 by 1
           do
              (when-let* ((p (piece-at-point game x y))
                         (_ (not (and (eq (piece-type p) 'king) (eq (piece-color p) (if (eq by 'white) 'black 'white))))))
                (if (and (eq (piece-color p) by) (or (eq (piece-type p) 'queen) (eq (piece-type p) 'bishop)))
                    (return-from brk 'bishop4)
                    (return-from brk nil)))))

   ;; (when-let ((p (piece-at-point game (- px 1) (- py 2))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (+ px 1) (- py 2))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (+ px 2) (- py 1))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (+ px 2) (+ py 1))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (+ px 1) (+ py 2))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (- px 1) (+ py 2))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (- px 2) (+ py 1))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))
   ;; (when-let ((p (piece-at-point game (- px 2) (- py 1))))
   ;;   (and (eq (piece-type p) 'knight) (eq (piece-color p) by)))

   (when-let ((p (piece-at-point game (- px 1) (- py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game px (- py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game (+ px 1) (- py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game (+ px 1) py)))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game (+ px 1) (+ py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game px (+ py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game (- px 1) (+ py 1))))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   (when-let ((p (piece-at-point game (- px 1) py)))
     (and (eq by (piece-color p)) (eq (piece-type p) 'king)))
   ))

(defun fb1-king-of (fb1)
  (declare (type fast-board-1 fb1)
           (values place place))
  (loop for y from 0 below 8 do
    (loop for x from 0 below 8 do
      (when (fast:bit-set-p (fb-king fb1) (+ x (* y 8)))
        (return-from fb1-king-of (values x y)))))
  (error "King couldn't be found in ~a" fb1)) ;; TODO: this should not be an error

(defun fb-point-checked-p (fb px py by)
  (declare (type fast-board fb)
           (type fixnum px py)
           (type symbol by)
           )
           ;; (values boolean))
  (when (and (>= px 0) (< px 8) (>= py 0) (< py 8))
    (let* ((bb (ash 1 (- 63 (* py 8) px)))
           (fb1 (if (eq by 'white) (fb-white fb) (fb-black fb))))
      (or
       (when (= (logand bb (fb-make-check-board fb by)) bb)
           'pawn-or-knight
           )
       (when
        (let ((rm (fb-generate-rook-moves fb px py (if (eq by 'white) 'black 'white))))
          (or
           (not (= 0 (logand rm (fb-rook fb1))))
           (not (= 0 (logand rm (fb-queen fb1))))))
         'rook-or-queen)
       (when
       (let ((bm (fb-generate-bishop-moves fb px py (if (eq by 'white) 'black 'white))))
         (or
          (not (= 0 (logand bm (fb-bishop fb1))))
          (not (= 0 (logand bm (fb-queen fb1))))))
         'bishop-or-queen)
       (let-values ((kx ky (fb1-king-of fb1)))
         (when
             (= (logand bb (fb-generate-king-area kx ky)) bb)
           'king
           ))))))

(defun new--point-checked-p (game px py by)
  (fb-point-checked-p (game-fb game) px py by))

;;; new--
;; real	0m7,302s
;; user	0m7,078s
;; sys	0m0,215s
;;; old--
;; real	0m6,407s
;; user	0m6,182s
;; sys	0m0,205s
;;
;; i'm keeping the new one bc of the bitboard usage
(defun point-checked-p (game px py by)
  (old--point-checked-p game px py by))

(declaim (sb-ext:maybe-inline point-checked-p))

(defun game-do-move (game piece mx my &key no-recache no-check-mates no-funcall upgrade-type no-send (no-display-check-mates nil))
  (declare (type game game)
           (type piece piece))
  ;; (warn "game-do-move move ~a to ~a" piece (list mx my))
  (when (not (move-possible-p piece mx my game))
    (warn "game-do-move called with invalid data: ~a -> (~a ~a)" piece mx my))

  (let ((inc-halfmove-p t)
        (upgrade-p (and
                    (eq (piece-type piece) 'pawn)
                    (or (= my 0) (= my 7)))))
    (when-let ((f (move-possible-p piece mx my game))
               (was-x (point-x (piece-point piece)))
               (was-y (point-y (piece-point piece))))
      (when-let ((p (piece-at-point game mx my)))
        (setf (game-halfmove-clock game) 0)
        (setf inc-halfmove-p nil)
        (setf (game-pieces game) (remove p (game-pieces game) :test #'equal)))

      (when (eq (piece-type piece) 'pawn)
        (setf inc-halfmove-p nil)
        (setf (game-halfmove-clock game) 0))

      (when (and
             (game-white-can-castle-kingside-p game)
             (eq (piece-type piece) 'rook)
             (= (point-x (piece-point piece)) 7)
             (= (point-y (piece-point piece)) 7))
        (setf (game-white-can-castle-kingside-p game) nil))

      (when (and
             (game-white-can-castle-queenside-p game)
             (eq (piece-type piece) 'rook)
             (= (point-x (piece-point piece)) 0)
             (= (point-y (piece-point piece)) 7))
        (setf (game-white-can-castle-queenside-p game) nil))

      (when (and
             (game-black-can-castle-kingside-p game)
             (eq (piece-type piece) 'rook)
             (= (point-x (piece-point piece)) 7)
             (= (point-y (piece-point piece)) 0))
        (setf (game-black-can-castle-kingside-p game) nil))

      (when (and
             (game-black-can-castle-queenside-p game)
             (eq (piece-type piece) 'rook)
             (= (point-x (piece-point piece)) 0)
             (= (point-y (piece-point piece)) 0))
        (setf (game-black-can-castle-queenside-p game) nil))

      (setf (game-en-passant-target-square game) nil)

      (let ((np (make-instance 'point :x mx :y my)))
        (push (list (piece-point piece) np) (game-move-history game))

        ;; move the damn thing to np (new point)
        (setf (piece-point piece) np))

      ;; TODO: this might be fixed somewhere else
      ;; TODO: when it's the player's turn allow them to choose,
      ;; otherwise, let the computer choose
      ; (when (and
      ;        (eq (piece-type piece) 'pawn)
      ;        (or (= 0 (point-y (piece-point piece)))
      ;            (= 7 (point-y (piece-point piece)))))
      ;   (setf (piece-type piece) 'queen))
      ;; ^- this got fixed somewhere else ~ kpm

      (when (eq (piece-type piece) 'king)
        (if (blackp piece)
            (progn
              (setf (game-black-can-castle-kingside-p game) nil)
              (setf (game-black-can-castle-queenside-p game) nil))
            (progn
              (setf (game-white-can-castle-kingside-p game) nil)
              (setf (game-white-can-castle-queenside-p game) nil))))

      (unless no-funcall
        (when (functionp f) ;; TODO: this is a freaky hack
          ;; this funcall can:
          ;;  * move rook after castling
          ;;  * set en-passant-target-square
          ;;  * delete a pawn after en passant
          ;;  * update the piece-type of a pawn after an upgrade
          (funcall f game)))

      ;; Received a upgrade-p packet, no-funcall is probably set and we can upgrade the
      ;; piece type manually
      (when (and upgrade-p upgrade-type)
        (setf (piece-type piece) upgrade-type))

      (when (and (game-connection game) (eq (game-side game) (game-turn game)) (not no-send))
        (net:write-packets
         (game-connection game)
         (net:make-client-packet
          'move
          :move-x1 was-x
          :move-y1 was-y
          :move-x2 mx
          :move-y2 my
          :move-upgrade-p upgrade-p
          :move-upgrade-type (piece-type piece))))

      (when (game-turn-black-p game)
        (incf (game-fullmove-clock game)))

      (when inc-halfmove-p
        (incf (game-halfmove-clock game)))

      (game-tick game)

      ;; (format t "evaluation of current position is: ~a~%" (evaluate-position game))

      (unless no-recache
        (setf (game-fb game) (game->fast-board game))
        (game-update-points-cache game)
        (game-update-possible-moves-cache game)
        )

      (unless no-check-mates
        (game-check-for-mates game :call-display (not no-display-check-mates))))))

(defun base-texture-of (thing)
  (if (vectorp thing)
      (aref thing 0)
      thing))

(defparameter maybe-drag/piece nil)
(defparameter maybe-drag/capturer (make-instance 'capturer :can-be-removed-p nil))

(defun maybe-drag (game &rest r)
  (declare (type game game)
           (ignore r))

  (when (game-in-progress-p game)
    (multiple-value-bind (px py)
        (coords->point (mouse-x) (mouse-y))
      (let ((px (maybe-reverse game px))
            (py (maybe-reverse game py))
            (bx (car *board-begin*))
            (by (cdr *board-begin*)))
        (cond
          ((mouse-pressed-p 0)  ; begin dragging
           (when (set-current-capturer! maybe-drag/capturer)
             ;; this when is spread like that so when you want to play both players you can implement the code for that easier
             (when (or (eq (game-turn game) (game-side game)) (null *threads*)) ;; TODO: assuming threads are not running in debug mode is bad
               (when-let ((p (piece-at-point game px py)))
                 (when (eq (piece-color p) (game-turn game))
                   (setf maybe-drag/piece p)))
               )
             ))
          ((and (mouse-released-p 0) maybe-drag/piece) ; end dragging
           (when (keys-can-be-captured-p maybe-drag/capturer)
             (delete-current-capturer!)
             (when (move-possible-p maybe-drag/piece px py game)
               (game-do-move game maybe-drag/piece px py))
             (setf maybe-drag/piece nil)))
          (maybe-drag/piece
           (when (keys-can-be-captured-p maybe-drag/capturer)
             (draw-rectangle
              (+ bx (* +piece-size+ (maybe-reverse game (point-x (piece-point maybe-drag/piece)))))
              (+ by (* +piece-size+ (maybe-reverse game (point-y (piece-point maybe-drag/piece)))))
              +piece-size+
              +piece-size+
              '(80 80 80 129))
             (draw-rectangle
              (+ bx (* +piece-size+ (maybe-reverse game px)))
              (+ by (* +piece-size+ (maybe-reverse game py)))
              +piece-size+
              +piece-size+
              '(80 80 80 80)))))))))

(defun highlight-possible-moves (game &rest r)
  (declare (type game game)
           (ignore r))

  (when-let ((p maybe-drag/piece)
             (bx (car *board-begin*))
             (by (cdr *board-begin*)))
    (dolist (pos (possible-moves-for game p))
      (draw-rectangle
       (+ bx (* +piece-size+ (maybe-reverse game (car pos))))
       (+ by (* +piece-size+ (maybe-reverse game (cadr pos))))
       +piece-size+
       +piece-size+
       +color-redish+))))

;; (defun describe-checked (game)
;;   (multiple-value-bind (px py)
;;       (coords->point (mouse-x) (mouse-y))
;;     (draw-text
;;      (if (point-checked-p game px py 'black)
;;          "yes"
;;          "no")
;;      0
;;      100
;;      32
;;      +color-white+)))
;; (add-draw-hook 'describe-checked)

(defparameter maybe-switch-sides/capturer (make-instance 'capturer))
(defun maybe-switch-sides (g)
  (declare (type game g))

  (when (set-current-capturer! maybe-switch-sides/capturer)
    (when (key-pressed-p #\S)
      (let ((s (if (eq (game-side g) 'white) 'black 'white)))
        (setf (game-side g) s)))
    (delete-current-capturer!)))

(defun maybe-draw-eval (&rest _)
  (declare (ignore _))
  (when-let ((e *current-board-evaluation*))
    (draw-text (format nil "opponent sees eval as: ~a" (float (/ e 100))) 10 10 21 +color-white+)))

(defparameter +hlm/last-from+ '(120 120 20 100))
(defparameter +hlm/last-to+   '(120 80  20 100))

(defun highlight-last-move (g &rest _)
  (declare (ignore _)
           (type game g))
  (when-let ((m (car (game-move-history g))))
    (draw-rectangle
     (+ (car *board-begin*) (* (point-x (car m)) +piece-size+))
     (+ (cdr *board-begin*) (* (point-y (car m)) +piece-size+))
     +piece-size+ +piece-size+ +hlm/last-from+)
    (draw-rectangle
     (+ (car *board-begin*) (* (point-x (cadr m)) +piece-size+))
     (+ (cdr *board-begin*) (* (point-y (cadr m)) +piece-size+))
     +piece-size+ +piece-size+ +hlm/last-to+)))

(add-draw-hook 'show-point-at-cursor)
(add-draw-hook 'maybe-drag)
(add-draw-hook 'highlight-possible-moves)
(add-draw-hook 'maybe-switch-sides)
(add-draw-hook 'maybe-draw-eval)
(add-draw-hook 'highlight-last-move)

(add-draw-hook 'gui:toplevel-console-listener)

;; (defparameter test-fen "5qk1/1q6/8/8/8/8/8/R3K2R w KQ-- - 0 1")

(defun maybe-receive-something (game)
  (declare (type game game))

  (when (and (not (eq (game-side game) (game-turn game))) (game-connection game))
    (when-let ((p (maybe-receive-packet (game-connection game))))
      (format t "got packet with type ~a~%" (packet->name p))
      (packet-case p
        (move (multiple-value-bind (x1 y1 x2 y2 upgrade-p upgrade-t)
                  (packet->movedata p)
                (format t "received movedata of ~a -> ~a~%" (lst->pos (list x1 y1)) (lst->pos (list x2 y2)))
                (game-do-move game (piece-at-point game x1 y1) x2 y2
                              :no-funcall upgrade-p
                              :upgrade-type upgrade-t)))
        (gdata
         (when (fast::bit-set-p (aref p 0) 7 :type-size 8)
           (let ((eval-data (net:from-s16 (aref p 2) (aref p 3))))
             (format t "got EVAL data from opponent: ~a~%" eval-data)
             (setf *current-board-evaluation* (if (eq (game-side game) 'white) (- eval-data) eval-data)))))
        (t (warn "Unhandled packet in maybe-receive-something ~a with type ~a" p (packet->name p)))))))

(defun initialize-game (game side conn &key no-overwrite-interactive)
  (setf (game-connection game) conn)
  (setf (game-side game) side)
  (setf (game-fb game) (game->fast-board game))
  (unless no-overwrite-interactive
    (setf (game-interactive-p game) nil))
  (game-update-points-cache game)
  (game-update-possible-moves-cache game))

(defun initialize-window! ()
  (init-window *window-width* *window-height* ":leszcz")
  ;; TODO: unset target fps when the engine is thinking or switch contexts or wtv
  (set-target-fps! 60)
  (set-exit-key! -1)

  (switch-textures-to 'pixel)

  (format t "white-texture-alist: ~a~%" white-texture-alist)
  (format t "black-texture-alist: ~a~%" black-texture-alist))

(defun game-main-loop (game side conn)
  (declare (type game game)
           (type symbol side))

  (when (not (window-ready-p))
    (initialize-window!))

  (initialize-game game side conn :no-overwrite-interactive t)

  (setf gui::toplevel-console/log nil)
  (setf gui::toplevel-console/state "")

  (setf *current-game* game)
  (format t "pieces: ~a~%" (game-pieces game))

  (let* ((txt (make-render-texture *window-width* *window-height*)))
    (begin-drawing)
    (begin-texture-mode txt)
    (clear-background +color-grayish+)
    (draw-game game)
    (end-texture-mode)
    (end-drawing)

    (let ((i (texture->image (nth 3 txt))))
      (unshade-screen i 60 :flip t)
      (unload-image! i)
      (unload-render-texture! txt)))

  (loop :while (not (window-close-p)) :do
    (setf *current-screen* (screen->image)) ;; TODO: this sucks
    (set-mouse-cursor! +cursor-normal+)
    ;; (when (key-pressed-p #\R)
    ;;   (setf game (fen->game +initial-fen+))
    ;;   (setf maybe-drag/piece nil)
    ;;   (setf (game-side game) nil)
    ;;   (game-update-possible-moves-cache game))

    (begin-drawing)

    (maybe-receive-something game)
    (clear-background +color-grayish+)
    (draw-game game)

    (dolist (h mainloop-draw-hooks)
      (funcall h game))

    (end-drawing)
    (unload-image! *current-screen*))
  (close-window))

;; Become a p2p "Master" server, accept a connection and begin game
(defun start-master-server ()
  (net:start-server
   #'(lambda (fen side conn)
       (game-main-loop (fen->game fen) side conn))))

(defun connect-to-master (&key (server "localhost") (username (symbol-name (gensym "username"))))
  (multiple-value-bind (fen side conn)
      (connect-to-server server username)
    (let ((g (fen->game fen)))
      (setf (game-interactive-p g) t)
      (game-main-loop g side conn))))

(defun maybe-move-bot (game &rest r)
  (declare (type game game)
           (ignore r))
  (when (game-in-progress-p game)
    (when (eq (game-turn game) (game-side game))
      (let-values ((eval pp mx my (game-search game 3)))
        (format t "bot chose position with eval ~a~%" eval)
        (when-let ((c (game-connection game)))
          (net:write-packets c (net:make-client-packet 'gdata :gdata-eval t :gdata-eval-data eval)))
        (game-do-move game (piece-at-point game (car pp) (cadr pp)) mx my)))))
      ;; (let* ((pre-ps (remove-if #'(lambda (p) (not (eq (piece-color p) (game-side game)))) (game-pieces game)))
      ;;        (ps (remove-if #'(lambda (p) (null (possible-moves-for game p))) pre-ps)))
      ;;   (when (> (length ps) 0)
      ;;     (let* ((chosen-piece (nth (random (length ps)) ps))
      ;;            (available-moves (possible-moves-for game chosen-piece))
      ;;            (chosen-move (nth (random (length available-moves)) available-moves)))
      ;;       (game-do-move game chosen-piece (car chosen-move) (cadr chosen-move))))))))

(defun cleanup-threads! ()
  (loop for thr in *threads* do
    (ignore-errors
     (sb-thread:terminate-thread thr)))
  (setf *threads* nil))

(defmacro thread (name &body b)
  `(let ((thr (sb-thread:make-thread
               #'(lambda ()
                   ,@b)
               :name ,name)))
     (prog1 thr
       (push thr *threads*))))

(defun %player-vs-bot ()
  (thread "bot thread (server)"
    (net:start-server
     #'(lambda (fen side conn)
         (let ((game (fen->game fen)))
           (initialize-game game side conn)
           (loop do
             (maybe-receive-something game)
             (maybe-move-bot game))))
     ))
     ;; :fen "8/8/4k3/8/8/PP6/KRp5/QB6 b - - 0 1" ;; <- TODO: upgrade to knight mates fen test
     ;; :fen "6k1/8/6b1/5q2/8/4n3/PP4PP/K6R w - - 0 1"))

  (sleep 1)
  (connect-to-master))
  ;; (sb-thread:join-thread
  ;;  (thread "user thread (client)" (connect-to-master))))

(defun %test-main ()
  ;; (let ((g (fen->game "r1b1k1nr/ppp2ppp/5q2/3Pp3/2B5/2N2N2/PPPPn1PP/R1BQ1RK1 w kq - 3 9")))
  (let ((g (fen->game "N7/8/4k3/8/8/PP6/KRp5/QB6 w - - 0 1")))
    (initialize-game g 'white nil)
    (setf (game-interactive-p g) t)
    (game-main-loop g 'white nil)))

(defparameter menu/bg-light '(#x2e #x2e #x2e #xff))
(defparameter menu/bg-dark  '(#x22 #x22 #x22 #xff))
(defparameter menu/frame-ctr 0)
(defparameter menu/frame-ctr-magic 4)
(defparameter menu/frame-ctr-mod (* menu/frame-ctr-magic +piece-size+))

(defun f/ (a b)
  (floor (/ a b)))

(defun animate-menu-bg ()
  (loop for y from (- +piece-size+) to *window-height* by +piece-size+ do
    (loop for x from (- +piece-size+) to *window-width* by +piece-size+ do
      (let ((color (if (= (mod (+ (f/ y +piece-size+) (f/ x +piece-size+)) 2) 0)
                       menu/bg-light
                       menu/bg-dark))
            (n (f/ menu/frame-ctr menu/frame-ctr-magic)))
        (draw-rectangle (+ x n) (+ y n) +piece-size+ +piece-size+ color))))
  (setf menu/frame-ctr (mod (1+ menu/frame-ctr) menu/frame-ctr-mod)))

(defun shade--screen (screen n-frames func &key flip)
  (declare (type fixnum n-frames))
  (let ((step (ceiling (/ 255 n-frames)))
        (shade 0)
        (tx (image->texture screen)))
    (loop for i from 0 below n-frames do
      (begin-drawing)

      (clear-background (list 0 0 0 0))
      (if flip
          (draw-texture
           tx
           (floatize (list 0 0 *window-width* (- *window-height*)))
           (floatize (list 0 0 *window-width* *window-height*))
           (floatize '(0 0)) (float 0)
           (list 255 255 255 (min 255 (funcall (the function func) shade))))
          (draw-texture
           tx
           (floatize (list 0 0 *window-width* *window-height*))
           (floatize (list 0 0 *window-width* *window-height*))
           (floatize '(0 0)) (float 0)
           (list 255 255 255 (min 255 (funcall (the function func) shade)))))

      (incf shade step)
      (end-drawing))
    (unload-texture! tx)))

(defun shade-screen (screen n-frames &key flip)
  (shade--screen screen n-frames #'(lambda (x) (- 255 x)) :flip flip))

(defun unshade-screen (screen n-frames &key flip)
  (shade--screen screen n-frames #'(lambda (x) x) :flip flip))

(defun %main ()
  (when (not (window-ready-p))
    (initialize-window!))

  (let ((continuation nil))
    (let-values ((b1 w1 h1 (gui:make-button* "zagraj se na bota" :height 24 :font-data alagard-data :font-hash raylib::*alagard* :text-draw-fn #'draw-text-alagard)))
      (loop until (or (window-close-p) continuation) do
        (setf *current-screen* (screen->image)) ;; TODO: this sucks
        (set-mouse-cursor! +cursor-normal+)
        (begin-drawing)

        (clear-background +color-grayish+)
        (animate-menu-bg)

        (let ((r `(,(float (car *board-begin*)) ,(float (cdr *board-begin*)) 512.0 256.0))
              (r2 `(,(+ (float (car *board-begin*)) 128) ,(+ (float (cdr *board-begin*)) 64) 256.0 128.0)))
          (draw-texture
           (cdr (assoc (if (point-in-rect-p (floatize (mouse-pos-1)) r2) 'leszcz2 'leszcz1) leszcz-logos-alist))
           '(0.0 0.0 512.0 256.0)
           r
           (floatize (list 0 0))
           (float 0)
           +color-white+))

        (funcall b1
                 (- (/ *window-width* 2) (/ w1 2))
                 (/ *window-height* 2)
                 #'(lambda (_)
                     (declare (ignore _))
                     (setf continuation #'%player-vs-bot)))
        (end-drawing)
        (unless continuation
          (unload-image! *current-screen*))))
    (when continuation
      (shade-screen *current-screen* 10)
      (funcall continuation))))

(defun main ()
  (unwind-protect
       (sb-int:with-float-traps-masked ;; TODO: weird untraceable problems on ms windows
           (:invalid :overflow :underflow :divide-by-zero :inexact)
         (%main))
    (cleanup-threads!)
    (when (window-ready-p)
      (close-window))))
