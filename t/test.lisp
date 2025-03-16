(defpackage leszcz-test
  (:use :common-lisp :alexandria :leszcz :leszcz-constants :leszcz-types :prove :raylib :net :fast))

(in-package :leszcz-test)

(setf leszcz-constants:*debug* t)
(setf prove:*enable-colors* nil)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *-we-debuggin* nil)
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

(plan 32)

(diag "Testing FEN serialization/deserialization")

(is
 +initial-fen+
 (leszcz::game->fen (leszcz::fen->game +initial-fen+)))

(let ((g (leszcz::fen->game "8/8/5k2/8/8/8/1q6/K2n4 w - - 0 1")))
  (leszcz::initialize-game g 'white nil)
  (leszcz::game-check-for-mates g :call-display nil)
  (is (leszcz::possible-moves-for g (leszcz::king-of g 'white)) nil)
  (is (game-result g) 'leszcz-constants:black))

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

          (push g* games))))
    games))

(diag "Testing move generation (perft)")

(defparameter *depth* 2)

;; (defparameter *expected-number-of-moves* '(20 400 8902 197281))
;; (defparameter *test-fen* +initial-fen+)

;; (defparameter *expected-number-of-moves* '(48 2039 97862 4085603))
;; (defparameter *test-fen* "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0")

(defparameter *expected-number-of-moves* '(14 191 2812 43238 674624))
(defparameter *test-fen* "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")

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

     ;; (with-open-file (str "fens.txt"
     ;;                      :direction :output
     ;;                      :if-exists :supersede
     ;;                      :if-does-not-exist :create)
     ;;   (loop for g in games do
     ;;     (format str "~a (~a)~%" (leszcz::game->fen g) (car (game-move-history g)))))

     (when *-we-debuggin*
       (let ((fen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N5/PPPBBPQP/R4K1R b kq - 0 1"))
         (setf games (list (fen->game* fen)))
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

(diag "Testing network packet generators & packet-case")

(defmacro test-packet-builder-and-packet-case (sym)
  `(let ((_s (net:make-client-packet ',sym :pgame-nick "" :move-x1 0 :move-x2 0 :move-y1 0 :move-y2 0)))
     (packet-case (car _s)
       (,sym (is ',sym ',sym))
       (t
        (is 'bad ',sym)))))

(defmacro test-packet-builders-and-packet-case ()
  (append
   '(progn)
   (loop for sym in '(hii gdata lgames pgame ping move)
         collect `(test-packet-builder-and-packet-case ,sym))))

(test-packet-builders-and-packet-case)

(diag "Testing various macros")

(is -17 (apply #'from-s16 (to-s16 -17)))
(is "abcdefg123" (rdata-packets->string (string->rdata "abcdefg123")))

(leszcz::enumerate 0 --a --b)
(is --a 0)
(is --b 1)

(loop for i from 0 below 16 do
  (let ((n (ash 1 i))
        (v 0))
    (fast::set-bit! v (- 63 i) t)
    (is v n)))

(finalize)
