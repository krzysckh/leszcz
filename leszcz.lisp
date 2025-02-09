;;; Leszcz entrypoint

(defpackage :leszcz
  (:use :common-lisp :leszcz-constants :leszcz-types :raylib :gui :alexandria :cl-ppcre :net)
  (:export
   main))

(in-package :leszcz)

(defun hasp (el l)
  (member el l :test #'equal))

(defmethod game-turn ((g game))
  (if (= (mod (game-ticker g) 2) 0) 'white 'black))

(defmethod game-turn-white-p ((g game))
  (eq (game-turn g) 'white))

(defmethod game-turn-black-p ((g game))
  (eq (game-turn g) 'black))

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

;; assuming a "vector" or "vector2" is a (list a b)
(defun v2+ (a b)
  (declare (type list a b))
  (list
   (+ (car a) (car b))
   (+ (cadr a) (cadr b))))

(defun v2- (a b)
  (declare (type list a b))
  (list
   (- (car a) (car b))
   (- (cadr a) (cadr b))))

(defun maybe-reverse (g pt)
  (declare (inline))
  (if (eq (game-side g) 'white)
      pt
      (- 7 pt)))

(defparameter draw-piece/anim-frame-ticker 0)
(defparameter draw-piece/anim-frame 0)
(defparameter draw-piece/piece-on-point nil)

(defun draw-piece (g p)
  (declare (type piece p)
           (type game g))

  (let* ((point (piece-point p))
         (x (* +piece-size+ (maybe-reverse g (point-x point))))
         (y (* +piece-size+ (maybe-reverse g (point-y point))))
         (al (if (whitep p) white-texture-alist black-texture-alist))
         (texture (cdr (assoc (piece-type p) al))))

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
         (floatize (list x y +piece-size+ +piece-size+))
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

  (let ((result "")
        (acc 0))
    (loop for y from 0 to 7 do
      (loop for x from 0 to 7 do
        (if-let ((p (piece-at-point g x y)))
          (progn
            (when (> acc 0)
              (setf result (concatenate 'string result (write-to-string acc))))
            (setf acc 0)
            (setf result (concatenate 'string result (string (piece->char p)))))
          (incf acc)))
      (when (> acc 0)
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
     :wck-p (find #\K castle-rules)
     :wcq-p (find #\Q castle-rules)
     :bck-p (find #\k castle-rules)
     :bcq-p (find #\q castle-rules)
     :en-passant-target-square (pos->lst (nth 3 l))
     :side 'white
     :connection nil
     )))

(defun draw-game (g)
  (declare (type game g))
  (let ((i (if (eq (game-side g) 'white) 0 1)))
    (loop for y from 0 to 7 do
      (loop for x from 0 to 8 do
        (let ((color (if (= (mod i 2) 0) +color-bg-light+ +color-bg-dark+)))
          (draw-rectangle (* +piece-size+ x) (* +piece-size+ y) +piece-size+ +piece-size+ color)
          (incf i)))))

  (let ((pieces (game-pieces g)))
    (dolist (p pieces)
      (draw-piece g p))))

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

(defun show-point-at-cursor (&rest r)
  (declare (ignore r))
  (multiple-value-bind (px py)
      (coords->point (mouse-x) (mouse-y))
    (draw-rectangle-lines (* px +piece-size+) (* py +piece-size+) +piece-size+ +piece-size+ +color-black+)))

(defun piece-at-point (game x y)
  (if-let ((ht (game-points-cache game)))
    (gethash (list x y) ht)
    (let ((pieces (game-pieces game)))
      (dolist (p pieces)
        (when (and (= (point-x (piece-point p)) x)
                   (= (point-y (piece-point p)) y))
          (return-from piece-at-point p)))
      nil)))

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
  (declare (optimize (speed 3) (safety 0)))
  (loop for m in moveset collect (v2+ m position)))

(defun generate-sliding-moves (game p moveset &key check-mode)
  (declare (type game game)
           (type piece p))
  (declare (optimize (speed 3) (safety 0)))

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
  (append
   ;; white
   (when (and
          (whitep p)
          (game-white-can-castle-kingside-p game)
          (null (piece-at-point game 5 7))
          (null (piece-at-point game 6 7))
          (not (point-checked-p game 5 7 'black))
          (not (point-checked-p game 6 7 'black))
          (eq (piece-type (piece-at-point game 7 7)) 'rook))
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
          (eq (piece-type (piece-at-point game 0 7)) 'rook))
     (list
      (list 2 7 #'(lambda (g*)
                    (let ((r (piece-at-point game 0 7)))
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
          (eq (piece-type (piece-at-point game 7 0)) 'rook))
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
          (eq (piece-type (piece-at-point game 0 0)) 'rook))
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
           (bb (shid ut/upgrade-bishop-texture 'bishop)))
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
          (draw-texture bg (floatize (list 0 0 *window-width* *window-height*)) (floatize (list 0 0 *window-width* *window-height*)) (floatize '(0 0)) (float 0) +color-white+)
          (funcall bq ut/upgrade-size ut/upgrade-size cleanup)
          (funcall br (+ (* 2 ut/upgrade-size) ut/pad-size) ut/upgrade-size cleanup)
          (funcall bn (+ (* 3 ut/upgrade-size) (* 2 ut/pad-size)) ut/upgrade-size cleanup)
          (funcall bb (+ (* 4 ut/upgrade-size) (* 3 ut/pad-size)) ut/upgrade-size cleanup)
          (set-mouse-cursor! +cursor-normal+)
          (end-drawing))
        (begin-drawing)
        (unload-texture! bg)
        clicked))))

(defun pre--possible-moves-for/upgrade (game p next-pos)
  (declare (type piece p))
  (if (and
       (eq (piece-type p) 'pawn)
       (or (= 0 (cadr next-pos))
           (= 7 (cadr next-pos))))
      (if (eq (game-side game) (piece-color p))
          (list
           (append next-pos (list #'(lambda (g*) ;; Ask interactively for upgrade type, also because asking interactively, just use p instead of looking for piece-at-point
                                      (setf (piece-type p) (ask-for-upgrade-type g* p))))))
          (let ((px (point-x (piece-point p)))
                (py (point-y (piece-point p))))
            (list ;; Generate all possible updgrade options and let "le computer" choose
             (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'queen)))))
             (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'rook)))))
             (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'bishop)))))
             (append next-pos `(,#'(lambda (g*) (let ((p (piece-at-point g* px py))) (setf (piece-type p) 'knight))))))))
      (list next-pos)))

(defun pre--possible-moves-for (game p check-mode)
  (multiple-value-bind (x y)
      (position-of p)
    (case (piece-type p)
      ;; TODO: ale swietny kod
      ;; TODO: no troche spaghetti
      ;; TODO: ja pierdole
      (pawn (let* ((m (when (not check-mode)
                        (filter-own-pieces
                         game p
                         (pre--possible-moves-for/upgrade game p (v2+ (list x y) (list 0 (if (whitep p) -1 1))))
                         :disallow-taking t)))
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
                                             #'(lambda (g*)
                                                 (setf (game-en-passant-target-square g*) pos))))))
                                   nil)
                               (if (and (blackp p) (= (point-y (piece-point p)) 1))
                                   (let ((pos (v2+ (list x y) '(0 2))))
                                     (list (append
                                            pos
                                            (list
                                             #'(lambda (g*)
                                                 (setf (game-en-passant-target-square g*) pos))))))
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
                                         #'(lambda (g*)
                                             (let ((p (piece-at-point g* (car ts) (cadr ts))))
                                               (setf (game-pieces g*)
                                                     (remove p (game-pieces g*) :test #'equal)))))
                                   m)
                                  ;; normal capturing
                                  (when-let ((p* (or check-mode (piece-at-point game ,x* ,y*))))
                                    (when (or check-mode (not (eq (piece-color p) (piece-color p*))))
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
      (let ((possible (possible-moves-for g p :recache t)))
        (when possible
          (setf (game-possible-moves-cache g)
                (acons (list (point-x (piece-point p))
                             (point-y (piece-point p)))
                       possible
                       (game-possible-moves-cache g)))))))

  ;; (format t "cache is ~a~%" (game-possible-moves-cache g))
  (values))

(defmethod game-update-points-cache ((g game))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (p (game-pieces g))
      (let ((point (piece-point p)))
        (setf (gethash (list (point-x point) (point-y point)) ht) p)))
    (setf (game-points-cache g) ht)))

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

(defun possible-moves-for (game p &key check-mode recache)
  (declare (type piece p)
           (type game game))
  (if (or recache check-mode)
      (let ((point (piece-point p)))
        (remove-if
         #'(lambda (pos)
             (when (not check-mode)
               (let ((p-was (piece-at-point game (car pos) (cadr pos))))
                 (setf (piece-point p) (make-instance 'point :x (car pos) :y (cadr pos)))
                 (let* ((king (king-of game (piece-color p)))
                        (king-point (piece-point king))
                        (points-cache (game-points-cache game)))
                   (setf (game-points-cache game) nil)
                   (when p-was
                     (setf (game-pieces game) (remove p-was (game-pieces game) :test #'equal)))
                   (prog1
                       (point-checked-p game (point-x king-point) (point-y king-point) (if (whitep king) 'black 'white))
                     (setf (piece-point p) point)
                     (setf (game-points-cache game) points-cache)
                     (when p-was
                       (push p-was (game-pieces game))))))))
         (remove-if
          #'(lambda (pos)
              (or
               (< (car pos) 0)
               (< (cadr pos) 0)
               (> (car pos) 7)
               (> (cadr pos) 7)))
          (pre--possible-moves-for game p check-mode))))
      (let ((p (piece-point p)))
        (cdr (assoc (list (point-x p) (point-y p)) (game-possible-moves-cache game) :test #'equal)))))

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
        (if (eq (piece-type p) 'king)
            (let ((x (point-x (piece-point p)))
                  (y (point-y (piece-point p))))
              (when (and (or (= px x)
                             (= px (- x 1))
                             (= px (+ x 1)))
                         (or (= py y)
                             (= py (- y 1))
                             (= py (+ y 1))))
                (return-from brk t)))
            (when (move-possible-p p px py game :check-mode t)
              ;; (format t "point (~a, ~a) is checkable by ~a (~a)~%" px py p (piece-type p))
              (return-from brk t))))
      nil)))

(defun game-do-move (game piece mx my &key no-recache no-check-mates)
  (declare (type game game)
           (type piece piece))
  ;; (warn "game-do-move move ~a to ~a" piece (list mx my))
  (when (not (move-possible-p piece mx my game))
    (warn "game-do-move called with invalid data: ~a -> (~a ~a)" piece mx my))

  (let ((inc-halfmove-p t))
    (when-let ((f (move-possible-p piece mx my game)))
      (when-let ((p (piece-at-point game mx my)))
        (setf (game-halfmove-clock game) 0)
        (setf inc-halfmove-p nil)
        (setf (game-pieces game) (remove p (game-pieces game) :test #'equal)))

      (when (and (game-connection game) (eq (game-side game) (game-turn game)))
        (net:write-packets
         (game-connection game)
         (net:make-client-packet
          'move
          :move-x1 (point-x (piece-point piece))
          :move-y1 (point-y (piece-point piece))
          :move-x2 mx
          :move-y2 my)))

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

      (when (game-turn-black-p game)
        (incf (game-fullmove-clock game)))

      (when inc-halfmove-p
        (incf (game-halfmove-clock game)))

      (game-tick game)

      (when (eq (piece-type piece) 'king)
        (if (blackp piece)
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
        (funcall f game))

      (unless no-recache
        (game-update-points-cache game)
        (game-update-possible-moves-cache game))

      (unless no-check-mates
        (game-check-for-mates game)))))

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
            (py (maybe-reverse game py)))
        (cond
          ((mouse-pressed-p 0)  ; begin dragging
           (when (set-current-capturer! maybe-drag/capturer)
             ;; this when is spread like that so when you want to play both players you can implement the code for that easier
             (when (eq (game-turn game) (game-side game))
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
              (* +piece-size+ (maybe-reverse game (point-x (piece-point maybe-drag/piece))))
              (* +piece-size+ (maybe-reverse game (point-y (piece-point maybe-drag/piece))))
              +piece-size+
              +piece-size+
              '(80 80 80 129))
             (draw-rectangle
              (* +piece-size+ (maybe-reverse game px))
              (* +piece-size+ (maybe-reverse game py))
              +piece-size+
              +piece-size+
              '(80 80 80 80)))))))))

(defun highlight-possible-moves (game &rest r)
  (declare (type game game)
           (ignore r))

  (when-let ((p maybe-drag/piece))
    (dolist (pos (possible-moves-for game p))
      (draw-rectangle
       (* +piece-size+ (maybe-reverse game (car pos)))
       (* +piece-size+ (maybe-reverse game (cadr pos)))
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

(add-draw-hook 'show-point-at-cursor)
(add-draw-hook 'maybe-drag)
(add-draw-hook 'highlight-possible-moves)
(add-draw-hook 'maybe-switch-sides)

(add-draw-hook 'gui:toplevel-console-listener)

(defun load-textures ()
  (setf white-texture-alist nil)
  (setf black-texture-alist nil)
  (setf raylib:*font* (make-hash-table :test #'equal)) ;; reset *font* every texture reload

  ;; (setf raylib:*font* (make-font spleen-data ".otf" 18 1024))
  (raylib:load-font spleen-data 18)
  ;; TODO:
  ;; ;; TODO: czemu tekstury są tak rozpikselizowane lol
  ;; (set-texture-filter! texture +TEXTURE-FILTER-POINT+)
  (macrolet ((load* (data-list alist)
               `(dolist (e ,data-list)
                  (if (listp (cdr e))
                      (let ((textures (mapcar #'(lambda (data) (make-texture data ".png")) (cdr e))))
                        (push (cons (car e) (coerce textures 'vector)) ,alist))
                      (let ((texture (make-texture (cdr e) ".png")))
                        (push (cons (car e) texture) ,alist))))))
    (load* white-texture-data-list white-texture-alist)
    (load* black-texture-data-list black-texture-alist)

    (format t "loaded textures~%")))

;; (defparameter test-fen "5qk1/1q6/8/8/8/8/8/R3K2R w KQ-- - 0 1")

;; do not use this, this is only for eval in repl
(defparameter *current-game* nil)
(defparameter *current-screen* nil)

;; TODO: protocol HAS to include data about the chosen piece after upgrade
(defun maybe-receive-something (game)
  (declare (type game game))

  (when (not (eq (game-side game) (game-turn game)))
    (when-let ((p (maybe-receive-packet (game-connection game))))
      (packet-case p
        (move (multiple-value-bind (x1 y1 x2 y2)
                  (packet->movedata p)
                (format t "received movedata of (~a ~a) -> (~a ~a)~%" x1 y1 x2 y2)
                (game-do-move game (piece-at-point game x1 y1) x2 y2)))
        (t (error "Unhandled packet in maybe-receive-something ~a with type ~a" p (packet->name p)))))))

(defun initialize-game (game side conn)
  (setf (game-connection game) conn)
  (setf (game-side game) side)
  (game-update-points-cache game)
  (game-update-possible-moves-cache game))

(defun main-loop (game side conn)
  (declare (type game game)
           (type symbol side))

  (initialize-game game side conn)

  (setf gui::toplevel-console/log nil)
  (setf gui::toplevel-console/state "")

  (init-window *window-width* *window-height* ":leszcz")
  ;; TODO: unset target fps when the engine is thinking or switch contexts or wtv
  (set-target-fps! 60)
  (set-exit-key! -1)

  (load-textures)

  (format t "white-texture-alist: ~a~%" white-texture-alist)
  (format t "black-texture-alist: ~a~%" black-texture-alist)

  (setf *current-game* game)
  (format t "pieces: ~a~%" (game-pieces game))

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
       (main-loop (fen->game fen) side conn))))

(defun connect-to-master (&key (server "localhost") (username (symbol-name (gensym "username"))))
  (multiple-value-bind (fen side conn)
      (connect-to-server server username)
    (main-loop (fen->game fen) side conn)))

(defun maybe-move-bot (game &rest r)
  (declare (type game game)
           (ignore r))
  (when (game-in-progress-p game)
    (when (eq (game-turn game) (game-side game))
      (let* ((pre-ps (remove-if #'(lambda (p) (not (eq (piece-color p) (game-side game)))) (game-pieces game)))
             (ps (remove-if #'(lambda (p) (null (possible-moves-for game p))) pre-ps)))
        (when (> (length ps) 0)
          (let* ((chosen-piece (nth (random (length ps)) ps))
                 (available-moves (possible-moves-for game chosen-piece))
                 (chosen-move (nth (random (length available-moves)) available-moves)))
            (game-do-move game chosen-piece (car chosen-move) (cadr chosen-move))))))))

(defun main ()
  (sb-thread:make-thread
   #'(lambda ()
       (net:start-server
        #'(lambda (fen side conn)
            (let ((game (fen->game fen)))
              (initialize-game game side conn)
              (loop do
                (maybe-receive-something game)
                (maybe-move-bot game))))
        :fen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0")))

  (sleep 1)
  (connect-to-master))
