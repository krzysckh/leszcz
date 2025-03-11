;;; Leszcz entrypoint

(defpackage :leszcz
  (:use
   :common-lisp
   :local-time :bordeaux-threads :alexandria :cl-ppcre :org.shirakumo.file-select
   :leszcz-constants :leszcz-types :raylib :gui :net :fast)
  (:export
   main))

(in-package :leszcz)

(defvar *threads* nil)

;; do not use this, this is only for eval in repl
(defparameter *current-game* nil)
(defparameter *current-screen* nil)

(defparameter *current-board-evaluation* nil) ; <- this is used mostly for testing

(defun hasp (el l)
  (member el l :test #'equal))

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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note)) ;; meh this is not that important

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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((i (if (eq (game-side g) 'white) 0 1))
        (bx (car *board-begin*))
        (by (cdr *board-begin*)))
    (when-let ((s (game-opponent-username g)))
      (draw-text-alagard s (car *board-begin*) (- (cdr *board-begin*) 16) 16 +color-white+))
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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
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
  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (when (and (>= px 0) (< px 8) (>= py 0) (< py 8))
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
             +piece-size+ +piece-size+ +color-redish+)))))))

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

(defun bb->move-lst (bb)
  (declare (type (unsigned-byte 64) bb)
           (values list))
  (let ((l nil))
    (loop for y from 0 below 8 do
      (loop for x from 0 below 8 do
        (when (fast:bit-set-p bb (+ x (* y 8)) :type-size 64)
          (push `(,x ,y) l))))
    l))

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

(defun save-game-to-pgn (game path)
  (with-open-file (f path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format f (game->pgn game))))

(defun display-game-finish-menu (game text)
  (let-values ((bg (image->texture *current-screen*))
               (b w1 h1 (abtn "powrot do menu" :height 24))
               (pgn w2 h2 (abtn "eksportuj jako pgn" :height 16))
               (x y w h (values
                         (+ (car *board-begin*) 30)
                         (+ (cdr *board-begin*) 120)
                         (- (* 8 +piece-size+) 60)
                         (- (* 8 +piece-size+) 240)))) ; rect
    (with-continued-mainloop cont %main
      (draw-texture
       bg
       (floatize (list (car *board-begin*) (cdr *board-begin*) *board-size* *board-size*))
       (floatize (list (car *board-begin*) (cdr *board-begin*) *board-size* *board-size*))
       ;; (floatize (list 0 0 *window-width* *window-height*))
       (floatize '(0 0)) (float 0) +color-white+)
      (draw-rectangle x y w h +color-grayish+)
      (draw-rectangle-lines-2 (floatize (list x y w h)) 8.0 '(#xde #xde #xde #xff))
      (draw-text-alagard-centered
       text
       (/ *window-width* 2)
       (+ y 30)
       30
       +color-white+)

      (funcall
       b
       (- (/ *window-width* 2) (/ w1 2))
       (/ *window-height* 2)
       #'(lambda (_)
           (cleanup-threads!)
           (setf cont #'%main)))

      (funcall
       pgn
       (- (/ *window-width* 2) (/ w2 2))
       (+ (/ *window-height* 2) h1 40)
       #'(lambda (_) ; this does not update continuation
           (let-values ((path okp (org.shirakumo.file-select:new)))
             (when okp
               (save-game-to-pgn game path)))))
      )))

(defun display-win (game)
  (declare (type game game))
  (display-game-finish-menu
   game
   (format nil "Koniec Gry! ~a~%"
           (if (eq (game-result game) (game-side game))
               "Wygrana!"
               "Przegrana... :("))))

(defun display-draw (game &optional why)
  (declare (type game game))
  (display-game-finish-menu
   game
   (format nil "Remis! (~a)~%" why)))

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
  ;; (when call-display
  ;;   (sb-debug:backtrace))
  (let ((c (game-possible-moves-cache g))
        (k (king-of g (game-turn g))))
    (cond
      ((and (null c) (point-checked-p g (point-x (piece-point k)) (point-y (piece-point k))
                                      (if (whitep k) 'black 'white)))
       ;; mate
       (setf (game-result g) (if (eq (game-turn g) 'white) 'black 'white))
       (when call-display
         (display-win g)))
      ((null c)
       ;; stalemate
       (setf (game-result g) 'draw)
       (when call-display
         (display-draw g "pat")))
      ((>= (game-halfmove-clock g) 50)
       (setf (game-result g) 'draw)
       (when call-display
         (display-draw g "zasada 50 ruchów")))
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
;; TODO: is this used anywhere?
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

(defun point-checked-p (game px py by)
  (fb-point-checked-p (game-fb game) px py by))

(declaim #+sbcl(sb-ext:maybe-inline point-checked-p))

(defun game-do-move (game piece mx my &key
                                        no-recache
                                        no-check-mates
                                        no-funcall
                                        upgrade-type
                                        no-send
                                        (no-display-check-mates nil)
                                        no-history
                                        no-update-timers
                                        )
  (declare
   (type game game)
   (type piece piece)
   (ignore no-display-check-mates))
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
        (unless no-history
          (push
           (list (copy-piece piece)
                 (list (point-x (piece-point piece)) (point-y (piece-point piece)))
                 (list (point-x np) (point-y np)))
           (game-move-history game)))

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

      (unless no-update-timers
        (let ((now (local-time:timestamp-to-unix (local-time:now))))
          (if (game-turn-white-p game)
              (setf
               (game-time-white game)
               (- (game-time-white game) (- now (game-time-begin-turn game))))
              (setf
               (game-time-black game)
               (- (game-time-black game) (- now (game-time-begin-turn game)))))
          (setf (game-time-begin-turn game) now)))

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
        (game-check-for-mates game :call-display nil))))) ;(not no-display-check-mates))))))

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
             (when (or (eq (game-turn game) (game-side game)) *debug*)
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
  (when *debug*
    (when-let ((e *current-board-evaluation*))
      (draw-text (format nil "opponent sees eval as: ~a" (float (/ e 100))) 10 10 21 +color-white+))))

(defparameter +hlm/last-from+ '(120 120 20 100))
(defparameter +hlm/last-to+   '(120 80  20 100))

(defun highlight-last-move (g &rest _)
  (declare (ignore _)
           (type game g))
  (when-let ((m (car (game-move-history g))))
    (let* ((a (cadr m))
           (b (caddr m))
           (x1 (maybe-reverse g (car a)))
           (y1 (maybe-reverse g (cadr a)))
           (x2 (maybe-reverse g (car b)))
           (y2 (maybe-reverse g (cadr b))))
      (draw-rectangle
       (+ (car *board-begin*) (* x1 +piece-size+))
       (+ (cdr *board-begin*) (* y1 +piece-size+))
       +piece-size+ +piece-size+ +hlm/last-from+)
      (draw-rectangle
       (+ (car *board-begin*) (* x2 +piece-size+))
       (+ (cdr *board-begin*) (* y2 +piece-size+))
       +piece-size+ +piece-size+ +hlm/last-to+))))

;; TODO: taking (incorporate data into move history)
;; TODO: castling
;; TODO: ambiguous moves
(defun move->algebraic (p from to)
  (declare (type piece p)
           (type list from to)
           (values string))
  (let ((c (piece->char p)))
    (case (piece-type p)
      (pawn (lst->pos `(,(car to) ,(cadr to))))
      (king "TODO")
      (t
       (format nil "~a~a" c (lst->pos `(,(car to) ,(cadr to))))))))

(defparameter dmh/height 128)
(defparameter dmh/xpad 32)
(defparameter dmh/font-size 16)
(defparameter dmh/rect (list (+ (car *board-begin*) *board-size* dmh/xpad)
                             (- (+ (cdr *board-begin*) (/ *board-size* 2)) (/ dmh/height 2))
                             (- *window-width* (+ (car *board-begin*) *board-size* dmh/xpad) dmh/xpad)
                             dmh/height))
(defparameter dmh/show 7)

(defun draw-move-history (g &rest _)
  (declare (type game g)
           (ignore _))
  (apply #'draw-rectangle (append dmh/rect '((#x55 #x55 #x55 #xff))))

  (loop for m in (last (reverse (game-move-history g)) (+ (* dmh/show 2)
                                                          (mod (length (game-move-history g)) 2)))
        for i from 0 do
          (draw-text
           (apply #'move->algebraic m)
           (if (= (mod i 2) 0)
               (+ (car dmh/rect) 8)
               (+ (car dmh/rect) 8 (/ (nth 2 dmh/rect) 2)))
           (+ (cadr dmh/rect) (* dmh/font-size (floor (/ i 2))))
           dmh/font-size
           '(#xde #xde #xde #xff))))

(defparameter dt/font-size 32)

(defun draw-time (g &rest _)
  (declare (ignore _)
           (type game g))
  (when (eq (game-result g) 'in-progress)
    (let* ((wleft (if (game-turn-white-p g)
                      (- (game-time-white g) (- (local-time:timestamp-to-unix (local-time:now)) (game-time-begin-turn g)))
                      (game-time-white g)))
           (bleft (if (game-turn-black-p g)
                      (- (game-time-black g) (- (local-time:timestamp-to-unix (local-time:now)) (game-time-begin-turn g)))
                      (game-time-black g)))
           (wm (floor (/ wleft 60)))
           (ws (- wleft (* wm 60)))
           (bm (floor (/ bleft 60)))
           (bs (- bleft (* bm 60))))

      (when (<= wleft 0)
        (setf (game-result g) 'black)
        (when (eq (game-side g) 'white)
          (game-resign g))
        (display-win g))

      (when (<= bleft 0)
        (setf (game-result g) 'white)
        (when (eq (game-side g) 'black)
          (game-resign g))
        (display-win g))

      (draw-text
       (apply #'format (append '(nil "~a:~2,'0d") (if (eq 'white (game-side g)) `(,bm ,bs) `(,wm ,ws))))
       (car dmh/rect)
       (- (cadr dmh/rect) dt/font-size)
       dt/font-size
       '(#xde #xde #xde #xff))
      (draw-text
       (apply #'format (append '(nil "~a:~2,'0d") (if (eq 'white (game-side g)) `(,wm ,ws) `(,bm ,bs))))
       (car dmh/rect)
       (+ (cadr dmh/rect) (cadddr dmh/rect))
       dt/font-size
       '(#xde #xde #xde #xff))
      )))

(defun draw-icon (iname x y w h)
  (declare (type symbol iname)
           (type number x y w h))

  (let-values ((px py (mouse-pos))
               (txt (cdr (assoc iname icon-texture-alist)))
               (rec1 (floatize (list x y w h)))
               (rec2 (floatize (list (- x 1) (- y 1) (+ w 2) (+ h 2))))
               (mousep (point-in-rect-p (floatize (list px py)) rec2)))
    (when mousep
      (set-mouse-cursor! +cursor-pointer+))
    (draw-rectangle-rounded
     rec2
     0.2
     5
     (if mousep
         '(#x33 #x33 #x33 #xff)
         '(#x1e #x1e #x1e #xff)))
    (draw-texture
     txt
     '(0.0 0.0 32.0 32.0)
     rec1
     '(0.0 0.0) 0.0
     +color-white+)

    (and mousep (mouse-pressed-p 0))))

(defun game-surrender (g)
  (declare (type game g))
  (setf (game-result g) (if (eq 'white (game-side g)) 'black 'white))
  (when-let ((c (game-connection g)))
    (net:write-packets c (net:make-client-packet 'gdata :gdata-surrender t)))
  (when (game-interactive-p g)
    (display-win g)))

(defun game-resign (g)
  (game-surrender g))

(defun game-propose-or-accept-draw (g)
  (declare (type game g))
  (error "TODO: game-propose-or-accept-draw is not implemented yet."))

(defun game-propose-or-accept-takeback (g)
  (declare (type game g))
  (warn "TODO: game-propose-or-accept-takeback is not implemented yet.")
  (when-let ((c (game-connection g)))
    (write-packets c (make-client-packet 'gdata :gdata-takeback-p t))))
  ;; (error "TODO: game-propose-or-accept-takeback is not implemented yet."))

(defun draw-game-control-buttons (g &rest _)
  (declare (type game g)
           (ignore _))
  (let ((rec (list (car dmh/rect) (+ (cadr dmh/rect) (cadddr dmh/rect) dt/font-size) (caddr dmh/rect) 32)))
    (when (draw-icon 'flag (car rec) (cadr rec) 32 32)
      (game-surrender g))
    (when (draw-icon 'draw (+ (car rec) 32 8) (cadr rec) 32 32)
      (game-propose-or-accept-draw g))
    (when (draw-icon 'takeback (+ (car rec) 32 8 32 8) (cadr rec) 32 32)
      (game-propose-or-accept-takeback g))
    ))

(defun maybe-set-cursor (g &rest _)
  (declare (type game g)
           (ignore _))
  (let-values ((px py (coords->point (mouse-x) (mouse-y))))
    (when-let* ((_ (and (>= px 0) (< px 8) (>= py 0) (< py 8)))
                (piece (piece-at-point g px py))
                (moves (possible-moves-for g piece)))
      (set-mouse-cursor! +cursor-pointer+))))

(defun send-ping-to (g)
  (declare (type game g))
  (when-let ((c (game-connection g))
             (payl (random #xffff)))
    (setf (gethash payl *last-ping-ht*) (local-time:now))
    (net:write-packets c (net:make-client-packet 'ping :ping-payload payl))))

(defun maybe-send-ping (g &rest _)
  (declare (type game g)
           (ignore _))

  (when (key-pressed-p #\P)
    (send-ping-to g)))

(defparameter *arrow-color* '(#x50 #xd9 #x80 #xaa))

(defmacro all* (as pred &rest values)
  `(block _b
     (loop for ,as in (list ,@values) do
       (when (not ,pred)
         (return-from _b nil)))
     t))

;; TODO: v- to
(defun draw-arrow (x1 y1 x2 y2 &key (color *arrow-color*))
  (let-values ((px1 py1 (coords->point x1 y1))
               (px2 py2 (coords->point x2 y2)))
    (when (all* v (and (>= v 0) (< v 8)) px1 py1 px2 py2)
      (draw-line-1 (floatize (list (+ (car *board-begin*) (/ +piece-size+ 2) (* px1 +piece-size+))
                                   (+ (cdr *board-begin*) (/ +piece-size+ 2) (* py1 +piece-size+))))
                   (floatize (list (+ (car *board-begin*) (/ +piece-size+ 2) (* px2 +piece-size+))
                                   (+ (cdr *board-begin*) (/ +piece-size+ 2) (* py2 +piece-size+))))
                   10.0
                   color)
  )))

(defparameter *arrow-last-point* nil)

(defun maybe-draw-arrow (g &rest _)
  (declare (type game g)
           (ignore _))

  (cond
    ((mouse-pressed-p 1)
     (setf *arrow-last-point* (cons (mouse-x) (mouse-y))))
    ((mouse-down-p 1)
     (draw-arrow (car *arrow-last-point*) (cdr *arrow-last-point*) (mouse-x) (mouse-y)))
    (t
     (setf *arrow-last-point* nil))))

(defun draw-menu-button (g &rest _)
  (declare (type game g)
           (ignore _))

  (let-values ((btn w h (make-button* (cdr (assoc 'settings icon-texture-alist)) :height 32 :width 32 :no-bg t :no-pad t)))
    (funcall btn 10 10 (lambda (_) (configure-menu)))))

(add-draw-hook 'show-point-at-cursor)
(add-draw-hook 'maybe-drag)
(add-draw-hook 'highlight-possible-moves)
(add-draw-hook 'maybe-switch-sides)
(add-draw-hook 'maybe-draw-eval)
(add-draw-hook 'highlight-last-move)
(add-draw-hook 'draw-move-history)
(add-draw-hook 'draw-time)
(add-draw-hook 'draw-game-control-buttons)
(add-draw-hook 'maybe-set-cursor)
(add-draw-hook 'draw-menu-button)

(add-draw-hook 'maybe-send-ping)
;; (add-draw-hook 'maybe-draw-arrow)

(add-draw-hook 'gui:toplevel-console-listener)

;; (defparameter test-fen "5qk1/1q6/8/8/8/8/8/R3K2R w KQ-- - 0 1")

(defparameter *last-ping-ht* (make-hash-table)) ;; table of payl -> (local-time:now) of ping sent

(defun maybe-receive-something (game)
  (declare (type game game))

  ;; (when (and (not (eq (game-side game) (game-turn game))) (game-connection game))
  (when-let ((c (game-connection game)))
    (when-let ((p (maybe-receive-packet (game-connection game))))
      (format t "got packet with type ~a~%" (packet->name p))
      (packet-case p
        (move (multiple-value-bind (x1 y1 x2 y2 upgrade-p upgrade-t)
                  (packet->movedata p)
                (format t "received movedata of ~a -> ~a~%" (lst->pos (list x1 y1)) (lst->pos (list x2 y2)))
                (game-do-move game (piece-at-point game x1 y1) x2 y2
                              :no-display-check-mates (not (game-interactive-p game)) ; <- ja pierdole, -30 minut zycia przez debugowanie double free przez to  ~ kpm
                              :no-funcall upgrade-p
                              :upgrade-type upgrade-t)))
        (lgames
         (let* ((ncont  (logior (ash (aref p 2) 8) (aref p 3)))
                (_ (format t "ncont is: ~a~%" ncont))
                (unames (rdatas->list (receive-packets c ncont))))
           (format t "received unames: ~a~%" unames)))
        (gdata
         (when (fast:bit-set-p (aref p 0) 7 :type-size 8)
           (let ((eval-data (net:from-s16 (aref p 2) (aref p 3))))
             (format t "got EVAL data from opponent: ~a~%" eval-data)
             (setf *current-board-evaluation* (if (eq (game-side game) 'white) (- eval-data) eval-data))))
         (when (fast:bit-set-p (aref p 0) 6 :type-size 8)
           (format t "received SURRENDER~%")
           (setf (game-result game) (game-side game))
           (when (game-interactive-p game)
             (display-win game)))
         (when (fast:bit-set-p (aref p 1) 0 :type-size 8)
           (format t "received TAKEBACK-P~%")
           (warn "TAKEBACK-P is not implemented yet.")
           )
         (when (fast:bit-set-p (aref p 1) 1 :type-size 8)
           (format t "received TAKEBACK-OK~%")
           (warn "TAKEBACK-OK is not implemented yet.")
           )
         (when (fast:bit-set-p (aref p 1) 2 :type-size 8)
           (format t "received TAKEBACK-OK-OK w/ CONT~%")
           (format t "NEW FEN is said to be ~a~%" (rdata-packets->string (receive-packets (game-connection game) (aref p 3))))
           )
         (when (fast:bit-set-p (aref p 1) 3 :type-size 8)
           (format t "received nickname w/ CONT~%")
           (setf (game-opponent-username game) (rdata-packets->string (receive-packets c (aref p 3))))
           )
         (when (fast:bit-set-p (aref p 1) 7 :type-size 8)
           (error "Server decided to BAIL OUT (opponent disconnect / invalid data sent)."))
         )
        (ping
         (let ((res-p (fast:bit-set-p (aref p 0) 3 :type-size 8))
               (payl (logior (ash (aref p 2) 8) (aref p 3))))
           (if res-p
               (when-let ((stamp (gethash payl *last-ping-ht*)))
                 (format t "Ping: ~,2f ms~%" (* (timestamp-difference (local-time:now) stamp) 1000)))
               (net:write-packets
                (game-connection game)
                (net:make-client-packet 'ping
                                        :ping-response-p t
                                        :ping-payload payl)))))
        (t (warn "Unhandled packet in maybe-receive-something ~a with type ~a" p (packet->name p)))))))

(defun initialize-game (game side conn &key no-overwrite-interactive)
  (setf (game-connection game) conn)
  (setf (game-side game) side)
  (setf (game-fb game) (game->fast-board game))
  (unless no-overwrite-interactive
    (setf (game-interactive-p game) nil))
  (game-update-points-cache game)
  (game-update-possible-moves-cache game))

(defun game-main-loop (game side conn)
  (declare (type game game)
           (type symbol side))

  (maybe-initialize-window!)

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

    (format t "txt: ~a~%" txt)
    (let ((i (texture->image (nth 3 txt))))
      (unshade-screen i 60 :flip t)
      (unload-image! i)
      (unload-render-texture! txt)))

  (setf (game-time-begin-turn game) (local-time:timestamp-to-unix (local-time:now)))

  (loop :while (not (window-close-p)) :do
    (setf *current-screen* (screen->image)) ;; TODO: this sucks
    ;; (when (key-pressed-p #\R)
    ;;   (setf game (fen->game +initial-fen+))
    ;;   (setf maybe-drag/piece nil)
    ;;   (setf (game-side game) nil)
    ;;   (game-update-possible-moves-cache game))

    (begin-drawing)

    (clear-background +color-grayish+)
    (maybe-receive-something game)

    (draw-game game)

    (when (not (eq (game-result game) 'in-progress)) ; only display the game finished menu after a frame with the winning position is rendered
      (end-drawing)
      (unload-image! *current-screen*)               ; TODO: this sucks even more
      (setf *current-screen* (screen->image))        ; <- and this too
      (game-check-for-mates game :call-display t))

    (dolist (h mainloop-draw-hooks)
      (funcall h game))

    (end-drawing)

    (unload-image! *current-screen*))
  (when-let ((c (game-connection game)))
    (write-packets c (make-client-packet 'gdata :gdata-bail-out t))))

;; Become a p2p "Master" server, accept a connection and begin game
(defun start-master-server (&key (port net:+port+) (side 'white) (time 10) (fen +initial-fen+))
  (net:start-server
   #'(lambda (fen side conn time)
       (let ((g (fen->game fen)))
         (initialize-game g side conn)
         (setf (game-interactive-p g) t)
         (setf (game-time-white g) (* 60 time))
         (setf (game-time-black g) (* 60 time))
         (game-main-loop g side conn)))
   :fen fen
   :opponent-side (if (eq side 'white) 'black 'white)
   :time time
   :port port))

(defun connect-to-master (&key (server "localhost") (port net:+port+) (username (symbol-name (gensym "username"))))
  (multiple-value-bind (fen side conn time)
      (connect-to-server server username :port port)
    (format t "server declared ~a minutes per player~%" time)
    (let ((g (fen->game fen)))
      (setf (game-interactive-p g) t)
      (setf (game-time-white g) (* time 60))
      (setf (game-time-black g) (* time 60))
      (initialize-game g side conn :no-overwrite-interactive t)
      (game-main-loop g side conn))))

;; (setf *trace-p* t)
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (push #P"/home/kpm/common-lisp/tracer/" asdf:*central-registry*)
;;   (push (uiop:getcwd) asdf:*central-registry*)
;;   (asdf:load-system :tracer))

(defmacro maybe-trace (&body b)
  `(progn ,@b))

;; (defmacro maybe-trace (&body b)
;;   (append
;;    `(progn
;;       (tracer:with-tracing ("LESZCZ" "NET" "GUI" "LESZCZ-CONSTANTS")
;;         ,@b)
;;       (tracer:save-report "leszcz-trace.json"))))

(defun maybe-move-bot (game &key (book *book*))
  (declare (type game game))
  (when (game-in-progress-p game)
    (when (eq (game-turn game) (game-side game))
      (let-values ((eval pp mx my (game-search game 3 :book book)))
        (format t "bot chose position with eval ~a~%" eval)
        (when-let ((c (game-connection game)))
          (net:write-packets c (net:make-client-packet 'gdata :gdata-eval t :gdata-eval-data eval)))
        (maybe-trace ;; debugging
          (game-do-move
           game (piece-at-point game (car pp) (cadr pp)) mx my
           :no-display-check-mates t))))))

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
     (destroy-thread thr)))
  (setf *threads* nil))

(defmacro thread (name &body b)
  `(let ((thr (make-thread
               #'(lambda ()
                   ,@b)
               :name ,name)))
     (prog1 thr
       (push thr *threads*))))

(defun make-player-vs-bot (book)
  #'(lambda ()
      (let-values ((color port time fen _uname (%game-options-menu "Zagraj na bota")))
        (thread "bot thread (server)"
          (net:start-server
           #'(lambda (fen side conn time)
               (let ((game (fen->game fen)))
                 (initialize-game game side conn)
                 (loop while (eq (game-result game) 'in-progress) do
                   (maybe-receive-something game)
                   (maybe-move-bot game :book book))))
           :time time
           :fen fen
           :port port
           :opponent-side color
           )))
      ;; :fen "8/8/4k3/8/8/PP6/KRp5/QB6 b - - 0 1" ;; <- TODO: upgrade to knight mates fen test
      ;; :fen "6k1/8/6b1/5q2/8/4n3/PP4PP/K6R w - - 0 1"))

      (sleep 1)
      (connect-to-master)))
;; (sb-thread:join-thread
;;  (thread "user thread (client)" (connect-to-master))))

(defun %local-player-vs-player ()
  (let ((game (fen->game +initial-fen+)))
    (initialize-game game 'white nil)
    (setf (game-interactive-p game) t)
    (flet ((update-side (g)
             (setf (game-side g) (game-turn g))))
      (add-draw-hook #'update-side)
      (game-main-loop game 'white nil)
      (remove-draw-hook #'update-side))))

(defun %test-main ()
  ;; (let ((g (fen->game "r1b1k1nr/ppp2ppp/5q2/3Pp3/2B5/2N2N2/PPPPn1PP/R1BQ1RK1 w kq - 3 9")))
  (let ((g (fen->game "N7/8/4k3/8/8/PP6/KRp5/QB6 w - - 0 1")))
    (initialize-game g 'white nil)
    (setf (game-interactive-p g) t)
    (setf *debug* t)
    (game-main-loop g 'white nil)
    (setf *debug* nil)
    (close-window)))

(defun %host-game-menu ()
  (let-values ((color port time fen _uname (%game-options-menu "Hostuj w LAN")))
    (start-master-server
     :side color
     :port port
     :time time
     :fen fen)))

;; options = ((text type val) ...)
;;   where type is one of (input-box (choice-of exp ...))
;; (defun %game-options-menu (title options)

(defun %game-options-menu (title &key no-port (no-nick t))
  (declare (type string title))
  (let ((current-color 'white))
    (let-values ((portsym    (gensym "port"))
                 (timesym    (gensym "time"))
                 (nicksym    (gensym "nick"))
                 (port pw ph (gui:make-input-box portsym :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value "3317"))
                 (time tw th (gui:make-input-box timesym :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value "15"))
                 (nick nw nh (gui:make-input-box nicksym :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value (symbol-name (gensym (format nil "~auser" (machine-instance))))))
                 (ok ow oh   (abtn "Ok" :height 24))
                 (bw w1 h1   (abtn "jasne" :height 24))
                 (bb w2 h2   (abtn "ciemne" :height 24))
                 )
      (with-continued-mainloop cont %main
        (let ((y (cdr *board-begin*)))
          (upy y 150 0 (draw-text-alagard-centered title (/ *window-width* 2) y 80 '(#x33 #xda #xf5 #xff)))

          (unless no-port
            (upy y 0 0   (draw-text-alagard "port " 128 y 24 +color-white+))
            (upy y ph 30 (funcall port 230 y 150)))

          (upy y 0 0   (draw-text-alagard "kolor " 128 y 24 +color-white+))
          (upy y h1 30 (funcall (if (eq current-color 'white) bw bb)
                                230 y
                                #'(lambda (&rest _)
                                    (setf current-color (if (eq current-color 'white) 'black 'white)))))

          (upy y 0 0   (draw-text-alagard "czas" 128 y 24 +color-white+))
          (upy y th 30 (funcall time 230 y))

          (unless no-nick
            (upy y 0 0   (draw-text-alagard "nick " 128 y 24 +color-white+))
            (upy y nh 30 (funcall nick 230 y 150)))

          (funcall
           ok (/ *window-width* 2) y
           #'(lambda (&rest _) ;; can i cont like this? lmao
               (setf cont
                     #'(lambda ()
                         (return-from %game-options-menu
                           (values
                            current-color
                            (parse-integer (coerce (gethash portsym input-box/content-ht) 'string))
                            (parse-integer (coerce (gethash timesym input-box/content-ht) 'string))
                            +initial-fen+           ; TODO
                            (coerce (gethash nicksym input-box/content-ht) 'string)
                            )))))))))))

(defun %ask-uname ()
  (let-values ((nicksym    (gensym "nick"))
               (nick nw nh (gui:make-input-box nicksym :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value (symbol-name (gensym (format nil "~auser" (machine-instance))))))
               (ok ow oh   (abtn "Ok" :height 24))
               )
    (with-continued-mainloop cont %main
      (let ((y (cdr *board-begin*)))
        (upy y 150 0 (draw-text-alagard-centered "Wybierz nick" (/ *window-width* 2) y 80 '(#x33 #xda #xf5 #xff)))

        (upy y 0 0   (draw-text-alagard "nick " 128 y 24 +color-white+))
        (upy y nh 30 (funcall nick 230 y 150))

        (funcall
         ok (/ *window-width* 2) y
         #'(lambda (&rest _) ;; can i cont like this? lmao
             (setf cont
                   #'(lambda ()
                       (return-from %ask-uname (coerce (gethash nicksym input-box/content-ht) 'string))))))))))

(defun %online-host-menu ()
  (let-values ((color _ time fen uname (%game-options-menu "Hostuj w sieci internet" :no-port t :no-nick nil)))
    (net:connect-to-server
     *online-host*
     uname
     :online-handler #'(lambda (conn)
                         ;; (net:write-packets conn (net:make-client-packet 'lgames))
                         (net:universal-start-server
                          conn
                          #'(lambda (fen side conn time)
                              (let ((g (fen->game fen)))
                                (initialize-game g side conn)
                                (setf (game-interactive-p g) t)
                                (setf (game-time-white g) (* 60 time))
                                (setf (game-time-black g) (* 60 time))
                                (let ((okp nil))
                                  (loop until (or okp (window-close-p)) do
                                    (sleep 0.01)
                                    (%display-waiting-for-connection)
                                    (when-let ((p (maybe-receive-packet conn)))
                                      (packet-case p
                                        (ping (when (fast:bit-set-p (aref p 0) 4 :type-size 8)
                                                (setf okp t)))
                                        (t
                                         (warn "Unexpected packet ~a while waiting for WAKEUP from server: ~a." (packet->name p) p))))))
                                (write-packets conn (make-client-packet 'gdata :gdata-uname uname))
                                (game-main-loop g side conn)))
                          :fen fen
                          :opponent-side (if (eq color 'black) 'white 'black)
                          :time time)))))

(defun %online-join-menu ()
  (let ((uname (%ask-uname)))
    (net:connect-to-server
     *online-host*
     uname
     :online-handler
     #'(lambda (conn)
         (write-packets conn (make-client-packet 'lgames))
         (let ((chosen nil))
           (loop until (or chosen (window-close-p)) do
             (sleep 0.01)
             (when-let ((p (maybe-receive-packet conn)))
               (packet-case p
                 (lgames
                  (flet ((cont (nick)
                           (net:write-packets conn (net:make-client-packet 'pgame :pgame-nick nick))
                           (let-values ((fen side _ time (p2p-connect-and-return-fen-and-side-data conn)))
                             (format t "server declared ~a minutes per player~%" time)
                             (let ((g (fen->game fen)))
                               (setf (game-interactive-p g) t)
                               (setf (game-time-white g) (* time 60))
                               (setf (game-time-black g) (* time 60))
                               (initialize-game g side conn :no-overwrite-interactive t)
                               (write-packets conn (make-client-packet 'ping :ping-wakeup t))
                               (write-packets conn (make-client-packet 'gdata :gdata-uname uname))
                               (game-main-loop g side conn)))))
                    (let* ((ncont (logior (ash (aref p 2) 8) (aref p 3)))
                           (unames (rdatas->list (receive-packets conn ncont)))
                           (buttons (loop for u in unames
                                          collecting `(,u . ,(lambda () (cont u))))))
                      (format t "received unames: ~a~%" unames)
                      (%bmenu "wybierz gre" #'%main buttons)
                      )))))))))))

(defun %join-game-menu ()
  (let-values ((portsym (gensym "joinport"))
               (ipsym (gensym "joinip"))
               (port pw ph (gui:make-input-box portsym :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value (format nil "~a" net:+port+)))
               (ip   iw ih (gui:make-input-box ipsym   :width 128 :height 24 :text-draw-fn #'draw-text-alagard :default-value "localhost"))
               (ok   ow oh (gui:make-button* "Ok" :height 24 :font-data alagard-data :font-hash raylib::*alagard* :text-draw-fn #'draw-text-alagard)))
  (with-continued-mainloop cont %main
    (draw-text-alagard-centered "Dolacz do gry w LAN" (/ *window-width* 2) (cdr *board-begin*) 80 '(#x33 #xda #xf5 #xff))
    (draw-text-alagard "port: " 128 (+ (cdr *board-begin*) 150) 24 +color-white+)
    (funcall port 256 (+ (cdr *board-begin*) 150) 24 +color-white+)
    (draw-text-alagard "ip: " 128 (+ (cdr *board-begin*) 200) 24 +color-white+)
    (funcall ip 256 (+ (cdr *board-begin*) 200) 24 +color-white+)
    (funcall ok (/ *window-width* 2) (/ *window-height* 2) #'(lambda (&rest _)
                                                               (declare (ignore _))
                                                               (setf cont
                                                                     #'(lambda ()
                                                                         (connect-to-master
                                                                          :server (coerce (gethash ipsym input-box/content-ht) 'string)
                                                                          :port (parse-integer (coerce (gethash portsym input-box/content-ht) 'string))))))))))

(defun %display-waiting-for-connection ()
  (begin-drawing)
  (animate-menu-bg)
  (draw-text-alagard "oczekiwanie na połącznie..." 32 32 18 '(#xde #xde #xde #xff))
  (end-drawing))

(defun %info-menu ()
  (let-values ((scroll 0)
               (w1 h1 (measure-text-1 (load-font spleen-data 18) license-text-1 18.0 0.0))
               (w2 h2 (measure-text-1 (load-font spleen-data 18) license-text-2 18.0 0.0)))
    (with-continued-mainloop continuation %main
      (let ((y (- (cdr *board-begin*) scroll)))
        (with-scrolling scroll y
          ;; (upy y 80 20 (draw-text-alagard-centered "Info" (/ *window-width* 2) y 80 '(#x33 #xda #xf5 #xff)))
          (upy y 50 20 (draw-text-alagard-centered "Licencja" (/ *window-width* 2) y 50 '(#x33 #xda #xf5 #xff)))
          (upy y h1 20 (draw-text license-text-1 (- (/ *window-width* 2) (/ w1 2)) y 18 +color-white+))
          (upy y 50 20 (draw-text-alagard-centered "Licencje zewnetrzne" (/ *window-width* 2) y 50 '(#x33 #xda #xf5 #xff)))
          (upy y 20 10 (draw-text-centered "(kompatybilne z powyższą)" (/ *window-width* 2) y 20 '(#x33 #xda #xf5 #xff)))
          (upy y h2 20 (draw-text license-text-2 (- (/ *window-width* 2) (/ w2 2)) y 18 +color-white+))
          )))))

(defun %main ()
  (let-values ((b1 w1 h1 (abtn "online"  :height 24))
               (b2 w2 h2 (abtn "offline" :height 24))
               (b3 w3 h3 (abtn "info"    :height 24)))
    (let ((r `(,(float (car *board-begin*)) ,(float (cdr *board-begin*)) 512.0 256.0))
          (r2 `(,(+ (float (car *board-begin*)) 128) ,(+ (float (cdr *board-begin*)) 64) 256.0 128.0)))
    (with-continued-mainloop continuation %main
      (let ((y (/ *window-height* 2)))
        (draw-texture
         (cdr (assoc (if (point-in-rect-p (floatize (mouse-pos-1)) r2) 'leszcz2 'leszcz1) leszcz-logos-alist))
         '(0.0 0.0 512.0 256.0)
         r
         (floatize (list 0 0))
         (float 0)
         +color-white+)

      (upy y h1 32
        (funcall
         b1
         (- (/ *window-width* 2) (/ w1 2)) y
         #'(lambda (_)
             (declare (ignore _))
             (setf continuation #'(lambda ()
                                    (%bmenu
                                     "Online"
                                     #'%main
                                     `(("zahostuj w LAN" . ,#'%host-game-menu)
                                       ("dolacz w LAN" . ,#'%join-game-menu)
                                       ("zahostuj w sieci internet" . ,#'%online-host-menu)
                                       ("dolacz w sieci" . ,#'%online-join-menu)
                                       )))))))

      (upy y h2 32
        (funcall
         b2
         (- (/ *window-width* 2) (/ w2 2)) y
         #'(lambda (_)
             (declare (ignore _))
             (setf continuation #'(lambda ()
                                    (%bmenu
                                     "Offline"
                                     #'%main
                                     `(("zagraj se na bota" . ,%player-vs-bot)
                                       ("zagraj na arcymistra" . ,#'(lambda ()
                                                                      (%bmenu
                                                                       "Na arcymistrza"
                                                                       #'%bmenu
                                                                       `(("Garry Kasparov"       . ,%player-vs-kasparov)
                                                                         ("Bobby Fischer"        . ,%player-vs-fischer)
                                                                         ("Magnus Carlsen"       . ,%player-vs-carlsen)
                                                                         ("Jose Raul Capablanca" . ,%player-vs-capablanca)
                                                                         ("Hikaru Nakamura"      . ,%player-vs-nakamura)
                                                                         ("Paul Morphy"          . ,%player-vs-morphy)
                                                                         ("Viswanathan Anand"    . ,%player-vs-anand)
                                                                         ("Mikhail Tal"          . ,%player-vs-tal)
                                                                         ("Fabiano Caruana"      . ,%player-vs-caruana)
                                                                         ("Mikhail Botvinnik"    . ,%player-vs-botvinnik)
                                                                         ("Judit Polgar"         . ,%player-vs-polgarj)
                                                                         ("Alexander Alekhine"   . ,%player-vs-alekhine)))))
                                       ("lokalnie na zioma" . ,#'%local-player-vs-player))))))))

      (upy y h3 32
        (funcall
         b3
         (- (/ *window-width* 2) (/ w3 2)) y
         #'(lambda (_)
             (declare (ignore _))
             (setf continuation #'%info-menu))))
        )))))

(defmacro maybe-trap-floats (&body b)
  #+sbcl`(sb-int:with-float-traps-masked ;; TODO: weird untraceable problems on ms windows
             (:invalid :overflow :underflow :divide-by-zero :inexact)
           ,@b)
  #-sbcl `(progn ,@b))

(defun show-exception-interactively-and-continue (e)
  (let-values ((mesg (format nil "An unexcpected error has occurred: ~%~a~%" e))
               (btn w1 h1 (abtn "Ok" :height 24)))
    (with-continued-mainloop continuation %main
      (draw-text mesg 10 10 24 +color-white+)
      (funcall
       btn
       (/ *window-width* 2)
       (/ *window-height* 2)
       #'(lambda (_)
           (declare (ignore _))
           (setf continuation #'(lambda ()
                                  (cleanup-threads!)
                                  (main))))))))

(defmacro maybe-catch-all-exceptions (&body b)
  `(if *prod*
       (handler-case (progn ,@b)
         (t (e)
           (show-exception-interactively-and-continue e)))
       (progn ,@b)))

(defmacro maybe-catch-finish (&body b)
  `(handler-case (progn ,@b)
     (finalize-condition (e)
       t)))

(defun main ()
  (unwind-protect (maybe-catch-all-exceptions
                    (maybe-catch-finish
                      (maybe-trap-floats
                        (when (uiop/filesystem:file-exists-p "config.lisp")
                          (load "config.lisp"))
                        (%main))))
    (cleanup-threads!)
    (when (window-ready-p)
      (close-window))))
