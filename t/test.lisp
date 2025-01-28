(defpackage leszcz-test
  (:use :common-lisp :alexandria :leszcz :leszcz-constants :leszcz-types :prove :raylib))

(in-package :leszcz-test)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(plan 7)

(diag "Testing FEN serialization/deserialization")

(is
 +initial-fen+
 (leszcz::game->fen (leszcz::fen->game +initial-fen+)))

(let ((g (leszcz::fen->game "8/8/5k2/8/8/8/1q6/K2n4 w - - 0 1")))
  (leszcz::game-check-for-mates g :call-display nil)
  (is (leszcz::possible-moves-for g (leszcz::king-of g 'white)) nil)
  (is (game-result g) 'checkmate))

(defun copy-game (g)
  (leszcz::fen->game (leszcz::game->fen g)))

(defun n-possible-moves (g)
  (reduce #'+ (mapcar #'length (mapcar #'cdr (game-possible-moves-cache g)))))

(defparameter captures 0)
(defparameter checks 0)

(defun game-permute (g)
  (leszcz::game-update-points-cache g)
  (leszcz::game-update-possible-moves-cache g)

  (let ((games nil))
    (loop for p in (game-pieces g) do
      (let* ((pt (piece-point p))
             (px (point-x pt))
             (py (point-y pt)))
        (loop for m in (leszcz::possible-moves-for g p) do
          (let* ((g* (copy-game g))
                 (p (leszcz::piece-at-point g* px py)))
            (leszcz::game-update-points-cache g*)
            (setf (game-possible-moves-cache g*) (game-possible-moves-cache g))

            (leszcz::game-do-move
             g*
             p
             (car m) (cadr m)
             :no-recache t
             :no-check-mates t)

          (when (not (= (length (game-pieces g*)) (length (game-pieces g))))
            (incf captures))

          (let ((k1 (leszcz::king-of g* 'white))
                (k2 (leszcz::king-of g* 'black)))
            (when (or (leszcz::point-checked-p
                       g*
                       (leszcz-types:point-x (leszcz-types:piece-point k1))
                       (leszcz-types:point-y (leszcz-types:piece-point k1))
                       'black)
                      (leszcz::point-checked-p
                       g*
                       (leszcz-types:point-x (leszcz-types:piece-point k2))
                       (leszcz-types:point-y (leszcz-types:piece-point k2))
                       'white))
              (incf checks)))

          (push g* games)
          ))))
  games))

(diag "Testing move generation")

(defparameter *expected-number-of-moves* '(20 400 8902 197281))

(let ((games (list (leszcz::fen->game +initial-fen+))))
  (loop for i from 0 to 3 do
    (setf captures 0)
    (setf checks 0)
    (setf games (mappend #'game-permute games))
    ;; TODO: figure out why the check count doesn't match https://www.chessprogramming.org/Perft_Results#Initial_Position and test that too
    ;; TODO: wow this is slow
    (is (length games) (nth i *expected-number-of-moves*))))

(finalize)
