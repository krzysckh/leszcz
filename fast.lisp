(defpackage :fast
  (:use :common-lisp :leszcz-constants :leszcz-types :alexandria :cl-ppcre)
  (:export
   bit-at
   bit-set-p
   game->fast
   ))

(in-package :fast)

;;; TODO: to będzie biblioteka która po przekonwertowaniu typu `game' na
;; coś bardziej optymalnego będzie przyspieszała generowanie ruchów itp
;; lol

;; (defstruct

(defun bit-at (n bit &key (type-size 64))
  (declare (type (unsigned-byte 64) n bit))
  (logand 1 (ash n (- (- type-size 1 bit)))))

(defun bit-set-p (n bit &key (type-size 64))
  (= (bit-at n bit :type-size type-size) 1))
