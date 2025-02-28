(in-package :leszcz)

;; inspired by https://www.chessprogramming.org/Simplified_Evaluation_Function
;; i tweaked most of the table values because whtat the hell was the guy smoking
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
                #(0  0   10  20  20  10  0  0)
                #(5 -5  -10  0  0   -10 -5 5)
                #(5  10  10 -20 -20 10  10 5)
                #(0  0   0  0   0   0   0  0)))
    (knight . #(#(-50 -40 -30 -30 -30 -30 -40 -50)
                #(-40 -20   0   0   0   0 -20 -4)
                #(-30   0  10  10  10  10   0 -3)
                #(-30   5  10  15  15  10   5 -3)
                #(-30   0  15  15  15  15   0 -3)
                #(-30   5  10  15  15  10   5 -3)
                #(-40 -20   0   5   5   0 -20 -4)
                #(-50 -20 -30 -30 -30 -30 -20 -5)))
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

(defparameter *rev-bonus-table*
  (mapcar #'(lambda (c) `(,(car c) . ,(reverse (cdr c)))) *bonus-table*))

(declaim (type short-float
               +pawn-value+
               +bishop-value+
               +knight-value+
               +rook-value+
               +queen-value+))

;; (defparameter *transposition-table* (make-hash-table :rehash-size 2 :size (ash 1 10)))

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
    ((or (eq (game-result game) 'white) (eq (game-result game) 'black))
     ;; (format t "pos evaluator found checkmate~%")
     (let ((king (king-of game (game-side game))))
       (if (point-checked-p
            game
            (point-x (piece-point king))
            (point-y (piece-point king))
            'white)
           +inf
           -inf)))
    ((eq (game-result game) 'stalemate)
     0)
    (t
     (let* ((ff (fast:game->fast-board game))
            (white-material (count-material-of (fb-white ff)))
            (black-material (count-material-of (fb-black ff)))
            (bonus (count-bonuses game)))
       (if (eq (game-side game) 'white)
           (- black-material white-material bonus)
           (+ (- white-material black-material) bonus))))))

;; TODO: unmake move !!!!!!!!!!!!
;; (defun fuck-copy-game (g)
;;   (let ((g* (fast-board->game (game->fast-board g))))
;;     (setf (game-side g*) (game-side g))
;;     (setf (game-black-can-castle-kingside-p g*) (game-side

;; https://www.chessprogramming.org/Negamax
;; https://www.chessprogramming.org/Alpha-Beta
(defun game--search (g depth alpha beta)
  ;; Add this hashing back when the engine is fast enough to be able to look further
  ; (when-let* ((v (gethash (hash-zobrist g) *transposition-table*)))
  ;   (format t "hit cache~%")
  ;   (return-from game--search
  ;     (values
  ;      (cadr v)
  ;      (car (caddr v))
  ;      (cadr (caddr v))
  ;      (caddr (caddr v)))))

  (if (= depth 0)
      (evaluate-position g)
      (let ((best-move nil)
            (best -inf))
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
                 :no-update-timers t
                 :no-display-check-mates t
                 :no-history t
                 ;; :no-recache t ;; <- dupa 2
                 )

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
        (progn
          ;; (setf (gethash (hash-zobrist g) *transposition-table*) (list depth best best-move))
          (values
           best
           (car best-move)
           (cadr best-move)
           (caddr best-move))))))

(setf *random-state* (make-random-state t))

(defun game-search (g depth &key (book *book*))
  (declare (type game g)
           (type fixnum depth))

  (leszcz::game-update-points-cache g)
  (leszcz::game-update-possible-moves-cache g)

  (when-let* ((z (hash-zobrist g))
              (moves (gethash z book))
              (move (random-elt moves))
              (p1 (car move))
              (p2 (cadr move))
              (p (piece-at-point g (car p1) (cadr p1)))
              (f (move-possible-p p (car p2) (cadr p2) g)))

    (format t "found move to ~a in book~%" (lst->pos p2))

    (return-from game-search
      (values 0 p1 (car p2) (cadr p2))))

  (game--search g depth -inf +inf))
