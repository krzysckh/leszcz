(defpackage leszcz-test
  (:use :common-lisp :alexandria :leszcz :leszcz-constants :leszcz-types :prove :raylib))

(in-package :leszcz-test)

(setf leszcz-constants:*debug* t)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *fuck-we-debuggin* nil)
(defparameter *trace-p* nil)
(defmacro maybe-trace (&body b)
  `(progn
     ,@b))

;; comment out to disable/enable tracing

;; (setf *trace-p* t)
;; (push #P"/home/kpm/common-lisp/tracer/" asdf:*central-registry*)
;; (push (uiop:getcwd) asdf:*central-registry*)
;; (asdf:load-system :tracer)
;; (defmacro maybe-trace (&body b)
;;   (append
;;    `(progn
;;       (tracer:with-tracing ("LESZCZ" "NET" "GUI" "LESZCZ-CONSTANTS")
;;         ,@b)
;;       (tracer:save-report "leszcz-trace.json"))))


(defun fen->game* (fen)
  (let ((g (leszcz::fen->game fen)))
    (leszcz::initialize-game g 'white nil)
    g))

(plan 7)

(diag "Testing FEN serialization/deserialization")

(is
 +initial-fen+
 (leszcz::game->fen (leszcz::fen->game +initial-fen+)))

(let ((g (leszcz::fen->game "8/8/5k2/8/8/8/1q6/K2n4 w - - 0 1")))
  (leszcz::initialize-game g 'white nil)
  (leszcz::game-check-for-mates g :call-display nil)
  (is (leszcz::possible-moves-for g (leszcz::king-of g 'white)) nil)
  (is (game-result g) 'checkmate))

;; (defun copy-game (g)
;;   (leszcz::fen->game (leszcz::game->fen g)))

(defun n-possible-moves (g)
  (reduce #'+ (mapcar #'length (mapcar #'cdr (game-possible-moves-cache g)))))

(defparameter captures 0)
(defparameter checks 0)

(defun game-permute (g)
  (leszcz::game-update-points-cache g)
  (leszcz::game-update-possible-moves-cache g)

  (let ((games nil))
    (loop for ms in (game-possible-moves-cache g) do
      (loop for m in (cdr ms) do
        (let* ((g* (leszcz::copy-game g))
               (p (leszcz::piece-at-point g* (caar ms) (cadar ms))))
          (leszcz::game-update-points-cache g*)
          ;; (leszcz::game-update-possible-moves-cache g*)
          (setf (game-possible-moves-cache g*) (game-possible-moves-cache g))

          (leszcz::game-do-move
           g*
           p
           (car m) (cadr m)
           :no-recache nil
           :no-check-mates t)

          ;; (when (not (= (length (game-pieces g*)) (length (game-pieces g))))
          ;;   (incf captures))

          ;; (let ((k1 (leszcz::king-of g* 'white))
          ;;       (k2 (leszcz::king-of g* 'black)))
          ;;   (when (or (leszcz::point-checked-p
          ;;              g*
          ;;              (leszcz-types:point-x (leszcz-types:piece-point k1))
          ;;              (leszcz-types:point-y (leszcz-types:piece-point k1))
          ;;              'black)
          ;;             (leszcz::point-checked-p
          ;;              g*
          ;;              (leszcz-types:point-x (leszcz-types:piece-point k2))
          ;;              (leszcz-types:point-y (leszcz-types:piece-point k2))
          ;;              'white))
          ;;     (incf checks)))

          (push g* games)
          )))
    games))

(diag "Testing move generation")

(defparameter *depth* 3)

;; (defparameter *expected-number-of-moves* '(20 400 8902 197281))
;; (defparameter *test-fen* +initial-fen+)

;; (defparameter *expected-number-of-moves* '(48 2039 97862 4085603))
;; (defparameter *test-fen* "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0")

(defparameter *expected-number-of-moves* '(14 191 2812 43238 674624))
(defparameter *test-fen* "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")

;; (let ((g (leszcz::fen->game *test-fen*)))
;;   (leszcz::game-update-points-cache g)
;;   (leszcz::game-update-possible-moves-cache g)
;;   (format
;;    t "possible moves: ~a~%"
;;    (length (game-permute g))))
;;    ;; (loop for m in (game-possible-moves-cache g)
;;    ;;       sum (length (cdr m)))))

;; (sb-ext:exit :code -1)

(maybe-trace
 (let ((g1 (leszcz::fen->game *test-fen*)))
   (leszcz::initialize-game g1 'white nil)
   (let ((games (list g1)))
     (loop for i from 0 to *depth* do
       (setf captures 0)
       (setf checks 0)
       (setf games (mappend #'game-permute games))
       ;; (format t "captures: ~a, checks: ~a~%" captures checks)
       (is (length games) (nth i *expected-number-of-moves*)))

     (with-open-file (str "fens.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
       (loop for g in games do
         (format str "~a (~a)~%" (leszcz::game->fen g) (car (game-move-history g)))))

     (when *fuck-we-debuggin*
       (let ((fuckfen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N5/PPPBBPQP/R4K1R b kq - 0 1"))
         (setf games (list (fen->game* fuckfen)))
         (setf (game-side (car games)) 'white)
         )

       (init-window *window-width* *window-height* ":leszcz")
       (set-target-fps! 60)
       (set-exit-key! -1)
       (load-textures)

       (loop :while (not (window-close-p)) :do
         (set-mouse-cursor! +cursor-normal+)

         (when (key-pressed-p #\A)
           (setf games (cdr games)))

         (begin-drawing)
         (clear-background +color-grayish+)
         (leszcz::draw-game (car games))

         ;; (format t "~a~%" (game-move-history (car games)))

         (leszcz::maybe-drag (car games))
         (leszcz::show-point-at-cursor (car games))
         (leszcz::highlight-possible-moves (car games))

         (let ((ll (game-move-history (car games))))
           (loop for l in (car ll) do
             (draw-rectangle
              (+ (car *board-begin*) (* (point-x l) +piece-size+))
              (+ (cdr *board-begin*) (* (point-y l) +piece-size+)) ;
              +piece-size+ +piece-size+ '(0 120 120 200)))
           (loop for l in (cadr ll) do
             (draw-rectangle
              (+ (car *board-begin*) (* (point-x l) +piece-size+))
              (+ (cdr *board-begin*) (* (point-y l) +piece-size+))
              +piece-size+ +piece-size+ +color-redish+)))

         (end-drawing))))))

(finalize)
