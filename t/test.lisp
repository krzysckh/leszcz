(defpackage leszcz-test
  (:use :common-lisp :leszcz :leszcz-constants :leszcz-types :prove))

(in-package :leszcz-test)

(plan 3)

(is
 +initial-fen+
 (leszcz::game->fen (leszcz::fen->game +initial-fen+)))

(let ((g (leszcz::fen->game "8/8/5k2/8/8/8/1q6/K2n4 w - - 0 1")))
  (leszcz::game-check-for-mates g :call-display nil)
  (is (leszcz::possible-moves-for g (leszcz::king-of g 'white)) nil)
  (is (game-result g) 'checkmate))

(finalize)
