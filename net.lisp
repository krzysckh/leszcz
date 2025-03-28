(defpackage :net
  (:use :common-lisp :leszcz-constants :leszcz-types :fast :alexandria :cl-ppcre :usocket)
  (:export
   start-server
   connect-to-server
   make-client-packet
   packet->movedata
   receive-packet
   maybe-receive-packet
   write-packets
   packet-of-type-p
   packet-name->type
   +port+
   packet->name
   packet-case
   to-s16
   from-s16
   ifz
   rdata-packets->string
   string->rdata
   receive-packets
   rdatas->list
   universal-start-server
   p2p-connect-and-return-fen-and-side-data ; believe it or not i have defined it before it became so useful

   +hii-type+
   +gdata-type+
   +lgames-type+
   +pgame-type+
   +ping-type+
   +move-type+
   +rdata-type+
   +invalid-type+

   ;; plain symbols
   hii
   gdata
   lgames
   pgame
   ping
   move
   rdata
   invalid
   ))

(in-package :net)

;; womp womp runtime vector dispatches
;; TODO: i could actually store packets in a simple-array of (unsigned-byte 8) but eh
(declaim #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))

(defconstant +port+ 3317)

(defconstant +hii-type+     #b00000000)
(defconstant +gdata-type+   #b00100000)
(defconstant +lgames-type+  #b01000000)
(defconstant +pgame-type+   #b01100000)
(defconstant +ping-type+    #b10000000)
(defconstant +move-type+    #b10100000)
(defconstant +rdata-type+   #b11000000)
(defconstant +invalid-type+ #b11100000)

(defun packet-name->type (sym)
  (declare (type symbol sym))
  (case sym
    (hii    +hii-type+)
    (gdata  +gdata-type+)
    (lgames +lgames-type+)
    (pgame  +pgame-type+)
    (ping   +ping-type+)
    (move   +move-type+)
    (rdata  +rdata-type+)
    (t
     +invalid-type+)))

(defun packet->name (p)
  (cond
    ((packet-of-type-p p +hii-type+) 'hii)
    ((packet-of-type-p p +gdata-type+) 'gdata)
    ((packet-of-type-p p +lgames-type+) 'lgames)
    ((packet-of-type-p p +pgame-type+) 'pgame)
    ((packet-of-type-p p +ping-type+) 'ping)
    ((packet-of-type-p p +move-type+) 'move)
    ((packet-of-type-p p +rdata-type+) 'rdata)
    (t 'invalid)))

;; (packet-case p
;;   (hii (do-something))
;;   (ping (do-something-else))
;;   (t))  ;; nye interestna
(defmacro packet-case (p &body cases)
  `(cond
     ,@(loop for c in cases collect `(,(if (eq t (car c))
                                           t
                                           `(packet-of-type-p ,p (packet-name->type (quote ,(car c)))))
                                      (progn
                                        ,@(cdr c))))))

(defun packet-of-type-p (packet type)
  (declare (type vector packet)
           (type (unsigned-byte 8) type))
  (let ((pt (logand #b11100000 (aref packet 0))))
    (= pt type)))

(defun safe-sref (s n)
  (declare (type string s)
           (type integer n))
  (if (>= n (length s))
      0
      (char-code (aref s n))))

(defun string->rdata (s)
  (declare (type string s))
  (let ((acc nil))
    (loop for i from 0 below (length s) by 3 do
      (let ((vec (make-sequence 'vector 4)))
        (setf (aref vec 0) (logior
                            +rdata-type+
                            (if acc #b00010000 0)))
        (setf (aref vec 1) (safe-sref s i))
        (setf (aref vec 2) (safe-sref s (+ i 1)))
        (setf (aref vec 3) (safe-sref s (+ i 2)))
        (setf acc (append acc (list vec)))))
    acc))

;; lst = (packet ...)
(defun rdatas->list (lst)
  (declare (type list lst)              ; mocne
           (values list))

  (let ((acc nil)
        (cur nil))
    (loop for p in lst do
      (when (not (fast:bit-set-p (aref p 0) 3 :type-size 8))
        (when cur
          (push (coerce cur 'string) acc))
        (setf cur nil))
      (setf cur (append
                 cur
                 (if (not (= 0 (aref p 1))) `(,(code-char (aref p 1))) nil)
                 (if (not (= 0 (aref p 2))) `(,(code-char (aref p 2))) nil)
                 (if (not (= 0 (aref p 3))) `(,(code-char (aref p 3))) nil))))

    (when cur
      (push (coerce cur 'string) acc))
    (reverse acc)))

(defun make-server-packet (type &key
                                  (hii-p2p t)
                                  (gdata-color 'white)
                                  (gdata-fen leszcz-constants:+initial-fen+)
                                  (gdata-time 10)
                                  )
  (let ((gdata-time* (min 32767 gdata-time)))
    (case type
      (hii   (list (vector (logior +hii-type+ (if hii-p2p #b00010000 0)) 0 0 0)))
      (gdata (let ((fen-rdata (string->rdata gdata-fen)))
               (append
                (list
                 (vector
                  (logior +gdata-type+ (if (eq gdata-color 'white) #b00010000 0))
                  (ash (logand #xff00 gdata-time*) -8)
                  (logand #x00ff gdata-time*)
                  (length fen-rdata)))
                fen-rdata)))
      (t
       (error "unsupported type for make-server-packet ~a" type)))))

(defmacro ifz (a b)
  `(if ,a ,b 0))

(defun to-s16 (n)
  (cond
    ((= n -inf) '(#xff #xff))
    ((= n +inf) '(#x7f #xff))
    (t (handler-case
           (let ((x (coerce (floor n) '(signed-byte 16))))
             (list (ash (logand #xff00 x) -8)
                   (logand #xff x)))
         (t () '(0 0))))))

(defun from-s16 (b1 b2)
  (let ((v (logior (ash b1 8) b2)))
    (if (> v #x7fff)
        (- v #x10000)
        v)))

(defun make-client-packet (type &key
                                  (hii-nickname "")
                                  move-x1 move-y1 move-x2 move-y2 move-upgrade-type move-upgrade-p
                                  gdata-drawp gdata-draw-ok gdata-surrender gdata-eval gdata-eval-data gdata-takeback-p
                                  gdata-takeback-ok gdata-takeback-ok-ok gdata-takeback-ok-fen gdata-uname gdata-bail-out
                                  (ping-payload (random #xffff)) ping-response-p ping-wakeup
                                  pgame-nick
                                  )
  (case type
    (hii (let ((nl-packets (string->rdata hii-nickname)))
           (append
            `(,(vector
                (logior +hii-type+ (ash (logand (length nl-packets) #b11111000) -3))
                (ash (logand (length nl-packets) #b111) 5)
                0
                0))
            nl-packets)))
    (move `(,(vector (logior +move-type+ (logand #xf move-x1))
                     (logior (ash (logand #xf move-y1) 4) (logand #xf move-x2))
                     (logior
                      (ash (logand #xf move-y2) 4)
                      (if move-upgrade-p
                          #b1000
                          #b0000)
                      (case move-upgrade-type
                        (queen  #b0000)
                        (rook   #b0010)
                        (knight #b0100)
                        (bishop #b0110)
                        (t      #b0000)))
                     0)))
    (gdata (let* ((cont-rdata (cond
                                (gdata-takeback-ok-ok (string->rdata gdata-takeback-ok-fen))
                                (gdata-uname (string->rdata gdata-uname))))
                  (_ (assert (<= (length cont-rdata) 255))) ; uhhh
                  (eval-or-ncont (if gdata-eval-data
                                     (to-s16 gdata-eval-data)
                                     (list
                                      #xff
                                      (length cont-rdata)))))
             `(,(vector
                 (logior
                  +gdata-type+
                  (ifz gdata-drawp     #b1000)
                  (ifz gdata-draw-ok   #b0100)
                  (ifz gdata-surrender #b0010)
                  (ifz gdata-eval      #b0001))
                 (logior
                  (ifz gdata-takeback-p     #b10000000)
                  (ifz gdata-takeback-ok    #b01000000)
                  (ifz gdata-takeback-ok-ok #b00100000)
                  (ifz gdata-uname          #b00010000)
                  (ifz gdata-bail-out       #b00000001)
                  )
                 (car eval-or-ncont)
                 (cadr eval-or-ncont))
               ,@cont-rdata)))
    (ping `(,(vector (logior +ping-type+ (ifz ping-response-p #b00010000) (ifz ping-wakeup #b00001000))
                     0
                     (ash (logand #xff00 ping-payload) -8)
                     (logand #xff ping-payload))))
    (lgames `(,(vector +lgames-type+ 0 0 0)))
    (pgame  (let ((cont-rdata (string->rdata pgame-nick)))
              `(,(vector +pgame-type+ 0 0 (length cont-rdata)) ,@cont-rdata)))
    (t
     (error "unsupported type for make-client-packet ~a" type))))

(defun write-packet (conn packet)
  (declare (type vector packet))
  (format t "will write packet ~a to ~a~%" packet conn)
  (let ((s (usocket:socket-stream conn)))
    (loop for byte across packet do
      (write-byte byte s))
    (force-output s)))

(defun receive-packet (conn)
  (let ((seq (make-sequence 'vector 4)))
    (loop until (usocket:wait-for-input conn :timeout 0.01)) ;; allow other threads to run
    (read-sequence seq (usocket:socket-stream conn))
    seq))

(defun maybe-receive-packet (conn)
  (ignore-errors
   (when (listen (usocket:socket-stream conn))
     (let ((seq (make-sequence 'vector 4)))
       (read-sequence seq (usocket:socket-stream conn))
       seq))))

(defun write-packets (conn packets)
  (format t "will write packets: ~a to ~a~%" packets (usocket:socket-stream conn))
  (dolist (p packets)
    (write-packet conn p)))

(defun receive-packets (conn n)
  (declare (type fixnum n))
  (loop for i from 1 to n collect (receive-packet conn)))

(defun rdata-packet->string (p)
  (declare (type vector p))
  (let ((c1 (aref p 1))
        (c2 (aref p 2))
        (c3 (aref p 3)))
    (coerce
     (append
      (if (= 0 c1) nil `(,(code-char c1)))
      (if (= 0 c2) nil `(,(code-char c2)))
      (if (= 0 c3) nil `(,(code-char c3))))
     'string)))

(defun rdata-packets->string (ps)
  (let ((s ""))
    (loop for p in ps do
      (when (not (packet-of-type-p p +rdata-type+))
        (error "shit, i expected RDATA packet, got ~a instead" p)) ;; <- TODO: this shouldn't be a fatal error
      (setf s (concatenate 'string s (rdata-packet->string p))))   ;; TODO: this is braindead
    s))

(defun receive-nickname (conn hii-packet)
  (let* ((nickname-length
           (logior
            (logand #b00011111 (aref hii-packet 0))
            (ash (logand #b11100000 (aref hii-packet 1)) -5)))
         (nickname-packets (receive-packets conn nickname-length))
         (nickname (rdata-packets->string nickname-packets)))
    (format t "[SERVER] got nickname: ~a~%" nickname)
    nickname))

(defun packet->movedata (p)
  (format t "up type: ~a~%" (ash (logand (aref p 2) #b00000110) -1))
  (if (packet-of-type-p p +move-type+)
      (let ((up-p (fast:bit-set-p (aref p 2) 4 :type-size 8)))
        (values
         (logand (aref p 0) #xf)
         (ash (logand (aref p 1) #xf0) -4)
         (logand (aref p 1) #x0f)
         (ash (logand (aref p 2) #xf0) -4)
         up-p
         (if up-p
             (case (ash (logand (aref p 2) #b00000110) -1)
               (#b00 'queen)
               (#b01 'rook)
               (#b10 'knight)
               (#b11 'bishop))
             nil)))
      (error "expected MOVE packet, got ~a instead" p)))

(defun universal-start-server (conn game-handler &key (fen +initial-fen+) (opponent-side 'white) (time 10))
  (write-packets conn (make-server-packet 'gdata :gdata-color opponent-side :gdata-fen fen :gdata-time time))

  (funcall game-handler fen (if (eq 'white opponent-side) 'black 'white) conn time)
  (format t "[SERVER] Closing p2p socket and connection~%"))

;; TODO: s/127.0.0.1/0.0.0.0/
(defun start-p2p-server (game-handler &key fen (opponent-side 'white) (time 10) port)
  (format t "[SERVER] starting p2p server @ port ~a~%" port)
  (with-socket-listener
      (sock "0.0.0.0" port :reuseaddress t :element-type '(unsigned-byte 8))
    ;; We're p2ping so accept 1 connection only
    (with-server-socket
        (conn (usocket:socket-accept sock))

      (write-packets conn (make-server-packet 'hii :hii-p2p t))

      (let* ((hii-back (receive-packet conn)))
        (assert (packet-of-type-p hii-back +hii-type+)))

      (universal-start-server conn game-handler :fen fen :opponent-side opponent-side :time time))))

      ;; (usocket:socket-close conn)
      ;; (usocket:socket-close sock))))

(defun start-server (game-handler &key
                                    (mode 'p2p)
                                    (fork nil)
                                    (fen +initial-fen+)
                                    (opponent-side 'white)
                                    (port +port+)
                                    (time 10))
  (declare (ignore fork))
  (case mode
    (p2p (start-p2p-server
          game-handler
          ;; :fen "7k/1Q6/R7/8/8/8/8/K7 w - - 0 1" ; fen
          :fen fen
          :opponent-side opponent-side
          :time time
          :port port))
    (t (error "Unknown server mode `~a'" mode))))

(defmacro if* (f a b)
  `(if (not (= ,f 0)) ,a ,b))

(defun p2p-connect-and-return-fen-and-side-data (conn)
  (let* ((gdata (receive-packet conn))
         (side (if* (logand #b00010000 (aref gdata 0)) 'white 'black))
         (time (logior (ash (aref gdata 1) 8) (aref gdata 2))))
    (values
     (rdata-packets->string (receive-packets conn (aref gdata 3)))
     side
     conn
     time)))

(defun connect-to-server (ip-or-conn nickname &key (port +port+) online-handler)
  (let ((conn (if (stringp ip-or-conn) (socket-connect ip-or-conn port :element-type '(unsigned-byte 8)) ip-or-conn)))
    (let ((hii (receive-packet conn)))
      (format t "[CLIENT] got hii: ~a (~a)~%" hii (packet->name hii))
      (if* (logand (aref hii 0) #b00010000)
          (progn
            (write-packets conn (make-client-packet 'hii)) ;; we're in p2p land, don't use a nick
            (p2p-connect-and-return-fen-and-side-data conn))
          (progn
            (write-packets conn (make-client-packet 'hii :hii-nickname nickname))
            (prog1 conn
              (when online-handler
                (funcall online-handler conn))))))))
