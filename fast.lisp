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

   fb-generate-bishop-moves
   fb-generate-knight-moves
   fb-generate-queen-moves
   fb-generate-rook-moves
   fb-generate-king-area
   fb-generate-king-moves

   fb-make-check-board
   ))

(in-package :fast)

;;; TODO: to będzie biblioteka która po przekonwertowaniu typu `game' na
;; coś bardziej optymalnego będzie przyspieszała generowanie ruchów itp
;; lol


(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defstruct (fast-board-1 (:conc-name fb-))
    (pawn   0 :type (unsigned-byte 64))
    (rook   0 :type (unsigned-byte 64))
    (knight 0 :type (unsigned-byte 64))
    (bishop 0 :type (unsigned-byte 64))
    (queen  0 :type (unsigned-byte 64))
    (king   0 :type (unsigned-byte 64))))

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
             (bit (the (unsigned-byte 64)
                       (+ (* (the (integer 0 8) (point-y pt)) 8) (the (integer 0 8) (point-x pt))))))
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

(defun fb--make-color-board (fb1)
  (declare (type fast-board-1 fb1)
           (values (unsigned-byte 64))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (logior
   (fb-pawn   fb1)
   (fb-rook   fb1)
   (fb-knight fb1)
   (fb-bishop fb1)
   (fb-queen  fb1)
   (fb-king   fb1)))

(defun fb-make-white-board (fb)
  (declare (type fast-board fb)
           (values (unsigned-byte 64))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (fb--make-color-board (fb-white fb)))

(defun fb-make-black-board (fb)
  (declare (type fast-board fb)
           (values (unsigned-byte 64))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (fb--make-color-board (fb-black fb)))

(defun fb-make-piece-board (fb)
  (declare (type fast-board fb)
           (values (unsigned-byte 64))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (logior (fb-make-black-board fb) (fb-make-white-board fb)))

(defun fb-display (n)
  (declare (type (unsigned-byte 64) n))
  (loop for i from 1 below 9 do
    (let ((x (logand #xff (ash n (- (- 64 (* 8 i)))))))
      (format t "~&~8,'0b~%" x))))

(defvar pawn-magic-L #.(lognot #x8080808080808080)) ;; pawns without far left side
(defvar pawn-magic-R #.(lognot #x0101010101010101))

(defconstant u64-max (the (unsigned-byte 64) #xffffffffffffffff))
(defmacro u64 (n)
  `(the (unsigned-byte 64) (logand u64-max ,n)))

;; https://www.chessprogramming.org/Knight_Pattern
(defun fb--knight-check-board (knights)
  (declare (type (unsigned-byte 64) knights)
           (values (unsigned-byte 64)))
  (let ((l1 (logand (u64 (ash knights -1)) #x7f7f7f7f7f7f7f7f))
        (l2 (logand (u64 (ash knights -2)) #x3f3f3f3f3f3f3f3f))
        (r1 (logand (u64 (ash knights 1))  #xfefefefefefefefe))
        (r2 (logand (u64 (ash knights 2))  #xfcfcfcfcfcfcfcfc)))
    (let* ((h1 (logior l1 r1))
           (h2 (logior l2 r2)))
      (logior
       (u64 (ash h1 16))
       (u64 (ash h1 -16))
       (u64 (ash h2 8))
       (u64 (ash h2 -8))))))

(declaim (inline fb--knight-check-board))

;; Color of checker
;; Warning: only produces check board of pawns and knights
(defun fb-make-check-board (fb color)
  (declare (type symbol color)
           (type fast-board fb))
  ;; fuck, i'd like to get rid of this ↓
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

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
          pawn-+2) ;; pawn R
     (fb--knight-check-board (fb-knight f1)))))

(defmacro fb1-at (fb1 x y)
  `(bit-at ,fb1 (+ ,x (* ,y 8))))

(defmacro fb1-set-p (fb1 x y)
  `(bit-set-p ,fb1 (+ ,x (* ,y 8))))

(defun fb-generate-knight-moves (fb px py color)
  (declare (type fast-board fb)
           (type fixnum px py)
           (values (unsigned-byte 64)))
  (let ((z 0)
        (ob (if (eq 'white color) (fb-make-white-board fb) (fb-make-black-board fb)))  ;; "own board"
        (eb (if (eq 'white color) (fb-make-black-board fb) (fb-make-white-board fb)))) ;; "enemy board"
    (setf (logbitpr z (+ px (* py 8))) t)
    (let ((cb (fb--knight-check-board z)))
      (logior
       (logand cb (lognot ob))
       (logand cb eb)))))

;; color is the color of a piece to generate moves for
(defun fb-generate-rook-moves (fb px py color)
  (declare (type fast-board fb)
           (type fixnum px py)
           (type symbol color)
           (values (unsigned-byte 64)))
  (let ((res 0)
        (ob (if (eq 'white color) (fb-make-white-board fb) (fb-make-black-board fb)))  ;; "own board"
        (eb (if (eq 'white color) (fb-make-black-board fb) (fb-make-white-board fb)))) ;; "enemy board"
    (block b
      (loop for x from (1- px) downto 0 do
        (when (fb1-set-p ob x py)
          (return-from b))
        (setf (logbitpr res (+ x (* py 8))) t)
        (when (fb1-set-p eb x py)
          (return-from b))))
    (block b
      (loop for x from (1+ px) to 7 do
        (when (fb1-set-p ob x py)
          (return-from b))
        (setf (logbitpr res (+ x (* py 8))) t)
        (when (fb1-set-p eb x py)
          (return-from b))))
    (block b
      (loop for y from (1- py) downto 0 do
        (when (fb1-set-p ob px y)
          (return-from b))
        (setf (logbitpr res (+ px (* y 8))) t)
        (when (fb1-set-p eb px y)
          (return-from b))))
    (block b
      (loop for y from (1+ py) to 7 do
        (when (fb1-set-p ob px y)
          (return-from b))
        (setf (logbitpr res (+ px (* y 8))) t)
        (when (fb1-set-p eb px y)
          (return-from b))))
    res))

(defun fb-generate-bishop-moves (fb px py color)
  (declare (type fast-board fb)
           (type fixnum px py)
           (type symbol color)
           (values (unsigned-byte 64)))
  (let ((res 0)
        (ob (if (eq 'white color) (fb-make-white-board fb) (fb-make-black-board fb)))  ;; "own board"
        (eb (if (eq 'white color) (fb-make-black-board fb) (fb-make-white-board fb)))) ;; "enemy board"
    (block b
      (loop for x from (1- px) downto 0
            for y from (1- py) downto 0
            do
               (when (fb1-set-p ob x y)
                 (return-from b))
               (setf (logbitpr res (+ x (* y 8))) t)
               (when (fb1-set-p eb x y)
                 (return-from b))))
    (block b
      (loop for x from (1- px) downto 0
            for y from (1+ py) to 7
            do
               (when (fb1-set-p ob x y)
                 (return-from b))
               (setf (logbitpr res (+ x (* y 8))) t)
               (when (fb1-set-p eb x y)
                 (return-from b))))
    (block b
      (loop for x from (1+ px) to 7
            for y from (1- py) downto 0
            do
               (when (fb1-set-p ob x y)
                 (return-from b))
               (setf (logbitpr res (+ x (* y 8))) t)
               (when (fb1-set-p eb x y)
                 (return-from b))))
    (block b
      (loop for x from (1+ px) to 7
            for y from (1+ py) to 7
            do
               (when (fb1-set-p ob x y)
                 (return-from b))
               (setf (logbitpr res (+ x (* y 8))) t)
               (when (fb1-set-p eb x y)
                 (return-from b))))
    res))

(defun fb-generate-queen-moves (fb px py color)
  (declare (type fast-board fb)
           (type fixnum px py)
           (type symbol color)
           (values (unsigned-byte 64)))
  (logior
   (fb-generate-bishop-moves fb px py color)
   (fb-generate-rook-moves   fb px py color)))

;; TODO: bitshift approach (knight-like)
(defun fb-generate-king-area (px py)
  (declare (type fixnum px py)
           (values (unsigned-byte 64)))
  (let ((z 0))
    (loop for m in '((0 . -1)
                     (1 . -1)
                     (1 . 0)
                     (1 . 1)
                     (0 . 1)
                     (-1 . 1)
                     (-1 . 0)
                     (-1 . -1))
          do
             (let* ((x (+ px (car m)))
                    (y (+ py (cdr m)))
                    (p (+ x (* y 8))))
               (when (and (>= x 0) (< x 8) (>= y 0) (< y 8))
                 (setf (logbitpr z p) t))))
    z))

(defun fb-generate-king-moves (fb px py color)
  (declare (type fast-board fb)
           (type fixnum px py)
           (type symbol color)
           (values (unsigned-byte 64)))
  (let ((ob (if (eq 'white color) (fb-make-white-board fb) (fb-make-black-board fb)))  ;; "own board"
        (eb (if (eq 'white color) (fb-make-black-board fb) (fb-make-white-board fb)))) ;; "enemy board"
    (let ((ka (fb-generate-king-area px py)))
      (logior
       (logand ka (lognot ob))
       (logand ka eb)))))

(defun bit-at (n bit &key (type-size 64))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (logand 1 (ash n (- (- type-size 1 bit)))))

(defun bit-set-p (n bit &key (type-size 64))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (logbitp (- type-size 1 bit) n))
