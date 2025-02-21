(in-package :leszcz)

;; https://www.chessprogramming.org/Simplified_Evaluation_Function
(define-constant +pawn-value+   100.0)
(define-constant +bishop-value+ 330.0)
(define-constant +knight-value+ 320.0)
(define-constant +rook-value+   500.0)
(define-constant +queen-value+  900.0)

;; defined for white
(defparameter *bonus-table*
  '((pawn   . #(#(0  0  0   0   0   0   0  0)
                #(50 50 50  50  50  50  50 50)
                #(10 10 20  30  30  20  10 10)
                #(5  5  10  25  25  10  5  5)
                #(0  0   0  20  20  0   0  0)
                #(5 -5  -10  0  0   -10 -5 5)
                #(5  10  10 -20 -20 10  10 5)
                #(0  0   0  0   0   0   0  0)))
    ;; (knight . #(#(-50 -40 -30 -30 -30 -30 -40 -50)
    ;;             #(-40 -20   0   0   0   0 -20 -4)
    ;;             #(-30   0  10  15  15  10   0 -3)
    ;;             #(-30   5  15  20  20  15   5 -3)
    ;;             #(-30   0  15  20  20  15   0 -3)
    ;;             #(-30   5  10  15  15  10   5 -3)
    ;;             #(-40 -20   0   5   5   0 -20 -4)
    ;;             #(-50 -40 -30 -30 -30 -30 -40 -5)))
    (bishop . #(#(-20 -10 -10 -10 -10 -10 -10 -20)
                #(-10   0   0   0   0   0   0 -10)
                #(-10   0   5  10  10   5   0 -10)
                #(-10   5   5  10  10   5   5 -10)
                #(-10   0  10  10  10  10   0 -10)
                #(-10  10  10  10  10  10  10 -10)
                #(-10   5   0   0   0   0   5 -10)
                #(-20 -10 -10 -10 -10 -10 -10 -20)))
    (rook   . #(#( 0   0   0   0   0   0   0   0)
                #( 5  10  10  10  10  10  10   5)
                #(-5   0   0   0   0   0   0  -5)
                #(-5   0   0   0   0   0   0  -5)
                #(-5   0   0   0   0   0   0  -5)
                #(-5   0   0   0   0   0   0  -5)
                #(-5   0   0   0   0   0   0  -5)
                #( 0   0   0   5   5   0   0   0)))
    (queen  . #(#(-20 -10 -10  -5  -5 -10 -10 -20)
                #(-10   0   0   0   0   0   0 -10)
                #(-10   0   5   5   5   5   0 -10)
                #( -5   0   5   5   5   5   0  -5)
                #(  0   0   5   5   5   5   0  -5)
                #(-10   5   5   5   5   5   0 -10)
                #(-10   0   5   0   0   0   0 -10)
                #(-20 -10 -10  -5  -5 -10 -10 -20)))
    ;; TODO: king
              ))

(defparameter *bot-random-state* (make-random-state))

(defmacro enumerate (start &body elements)
  (append
   '(progn)
   (loop for i from start
         for e in elements
         collect `(define-constant ,e ,i))))

(defun rand64 ()
  (the (unsigned-byte 64) (random (- (ash 1 64) 1) *bot-random-state*)))

(defun initialize-zobrist ()
  (setf *zobrist-table* (make-array '(64 13) :element-type '(unsigned-byte 64)))
  (loop for i from 0 below 64 do
    (loop for j from 0 below 12 do
      (setf (the (unsigned-byte 64) (aref *zobrist-table* i j)) (rand64))))
  (setf (the (unsigned-byte 64) *zobrist-black-to-move*) (rand64)))

(defun hash-zobrist (game)
  (declare (type game game)
           (values (unsigned-byte 64)))
  (let ((h (the (unsigned-byte 64) 0)))
    (when (game-turn-black-p game)
      (setf h (logxor h *zobrist-black-to-move*)))
    (loop for p in (game-pieces game) do
      (let ((i (+ (point-x (piece-point p)) (* 8 (point-y (piece-point p))))))
        (setf h (logxor h (aref *zobrist-table* i (piece->zobrist-hash-enum p))))))
    h))

(defparameter *transposition-table* (make-hash-table :hash-function #'hash-zobrist))

(defparameter *zobrist-table* nil)
(defparameter *zobrist-black-to-move* nil)

(enumerate 0
  z-white-pawn
  z-white-rook
  z-white-bishop
  z-white-knight
  z-white-queen
  z-white-king

  z-black-pawn
  z-black-rook
  z-black-bishop
  z-black-knight
  z-black-queen
  z-black-king)

(defun piece->zobrist-hash-enum (p)
  (declare (type piece p))
  (if (whitep p)
      (case (piece-type p)
        (pawn   z-white-pawn)
        (rook   z-white-rook)
        (bishop z-white-bishop)
        (knight z-white-knight)
        (queen  z-white-queen)
        (king   z-white-king))
      (case (piece-type p)
        (pawn   z-black-pawn)
        (rook   z-black-rook)
        (bishop z-black-bishop)
        (knight z-black-knight)
        (queen  z-black-queen)
        (king   z-black-king))))

(defparameter *rev-bonus-table*
  (mapcar #'(lambda (c) `(,(car c) . ,(reverse (cdr c)))) *bonus-table*))

(declaim (type short-float
               +pawn-value+
               +bishop-value+
               +knight-value+
               +rook-value+
               +queen-value+))

(defun count-material-of (ff)
  (declare (type fast-board-1 ff))

  (the short-float
       (+ (* +pawn-value+   (logcount (fb-pawn ff)))
          (* +bishop-value+ (logcount (fb-bishop ff)))
          (* +knight-value+ (logcount (fb-knight ff)))
          (* +rook-value+   (logcount (fb-rook ff)))
          (* +queen-value+  (logcount (fb-queen ff))))))

(defun count-bonuses (game)
  (declare (type game game))

  (loop for p in (game-pieces game)
        sum (let* ((pt (piece-point p)))
              ;; why no if-let*?
              (if-let ((tbl (cdr (assoc (piece-type p) (if (whitep p) *bonus-table* *rev-bonus-table*)))))
                (let ((v (aref (aref tbl (point-y pt)) (point-x pt))))
                  (if (whitep p) v (- v)))
                0))))

(defun evaluate-position (game)
  (declare (type game game))
  (cond
    ((eq (game-result game) 'checkmate)
     ;; (format t "pos evaluator found checkmate~%")
     (let ((king (king-of game 'black)))
       (if (point-checked-p
            game
            (point-x (piece-point king))
            (point-y (piece-point king))
            'white)
           sb-ext:short-float-positive-infinity
           sb-ext:short-float-negative-infinity)))
    ((eq (game-result game) 'stalemate)
     0)
    (t
     (let* ((ff (fast:game->fast-board game))
            (white-material (count-material-of (fb-white ff)))
            (black-material (count-material-of (fb-black ff)))
            (pawn-bonus (count-bonuses game)))
       (+ (- white-material black-material)
          pawn-bonus
          )))))

;; TODO: unmake move !!!!!!!!!!!!
;; (defun fuck-copy-game (g)
;;   (let ((g* (fast-board->game (game->fast-board g))))
;;     (setf (game-side g*) (game-side g))
;;     (setf (game-black-can-castle-kingside-p g*) (game-side

;; TODO: move to leszcz-types
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

;; https://www.chessprogramming.org/Negamax
;; https://www.chessprogramming.org/Alpha-Beta
(defun game--search (g depth alpha beta)
  ;; (format t "game--search w/ depth ~a (~a, ~a) of ticker ~a~%" depth alpha beta (game-ticker g))
  (if (= depth 0)
      (evaluate-position g)
      (let ((best-move nil)
            (best sb-ext:short-float-negative-infinity))
        (block brk
          (loop for ms in (game-possible-moves-cache g) do
            (loop for m in (cdr ms) do
              (let* ((g* (copy-game g))
                     (p (leszcz::piece-at-point g* (caar ms) (cadar ms))))
                (leszcz::game-update-points-cache g*)
                ;; (leszcz::game-update-possible-moves-cache g*) ;; <- dupa

                (setf (game-possible-moves-cache g*) (game-possible-moves-cache g))

                (game-do-move
                 g*
                 p
                 (car m) (cadr m)
                 :no-send t
                 :no-display-check-mates t
                 ;; :no-recache t ;; <- dupa 2
                 )

                (format t "in game--search state is: ~a~%" (game-result g*))

                (let ((score (* -1 (game--search g* (1- depth) (- beta) (- alpha)))))
                  (when (null best-move)
                    (setf best-move (append `((,(caar ms) ,(cadar ms))) m)))
                  (when (> score best)
                    (setf best score)
                    (when (> score alpha)
                      (setf best-move (append `((,(caar ms) ,(cadar ms))) m))
                      (setf alpha score)))
                  (when (>= score beta)
                    (return-from brk)))))))
        (values
         best
         (car best-move)
         (cadr best-move)
         (caddr best-move)))))

(defun game-search (g depth)
  (declare (type game g)
           (type fixnum depth))

  (leszcz::game-update-points-cache g)
  (leszcz::game-update-possible-moves-cache g)

  (game--search g depth sb-ext:short-float-negative-infinity sb-ext:short-float-positive-infinity))
