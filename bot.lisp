(in-package :leszcz)

;; (defun piece->value (p)
;;   (declare (type piece p))
;;   (case (piece-type p)
;;     (king 1000)
;;     (queen 9)
;;     (rook 5)
;;     (bishop 3)
;;     (knight 3)
;;     (pawn 1)))

(define-constant +pawn-value+ 100.0)
(define-constant +bishop-value+ 300.0)
(define-constant +knight-value+ 300.0)
(define-constant +rook-value+ 500.0)
(define-constant +queen-value+ 900.0)

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
            (black-material (count-material-of (fb-black ff))))
       (- white-material black-material)))))

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
              :point (piece-point p))))

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
             :possible-moves-cache nil
             :side (game-side g)
             :points-cache nil
             :halfmove-clock (game-halfmove-clock g)
             :fullmove-clock (game-fullmove-clock g)
             :result (game-result g)
             :connection (game-connection g))))

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
                (leszcz::game-update-possible-moves-cache g*) ;; <- dupa

                ;; (setf (game-possible-moves-cache g*) (game-possible-moves-cache g))

                (game-do-move
                 g*
                 p
                 (car m) (cadr m)
                 :no-send t
                 ;; :no-recache t ;; <- dupa 2
                 ;; :no-display-check-mates t)
                 )

                (let ((score (* -1 (game--search g* (1- depth) (- beta) (- alpha)))))
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
