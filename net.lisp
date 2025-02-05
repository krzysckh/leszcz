(defpackage :net
  (:use :common-lisp :leszcz-constants :leszcz-types :alexandria :cl-ppcre :usocket)
  (:export
   start-server
   connect-to-server
   make-client-packet
   packet->movedata
   receive-packet
   maybe-receive-packet
   write-packets
   packet-of-type-p
   +port+
   +rdata-type+
   +gdata-type+
   +hii-type+
   +move-type+

   ;; plain symbols
   gdata
   rdata
   hii
   move
   ))

(in-package :net)

(defconstant +port+ 3317)

(defconstant +rdata-type+ #b11000000)
(defconstant +gdata-type+ #b00100000)
(defconstant +hii-type+   #b00000000)
(defconstant +move-type+  #b10100000)

(defun packet-of-type-p (packet type)
  (declare (type vector packet)
           (type number type))
  (the boolean (= (logand (aref packet 0) type) type)))

(defun safe-sref (s n)
  (declare (type string s)
           (type number n))
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

(defun make-server-packet (type &key (hii-p2p t) (gdata-color 'white) (gdata-fen leszcz-constants:+initial-fen+))
  (case type
    (hii   (list (vector (logior +hii-type+ (if hii-p2p #b00010000 0)) 0 0 0)))
    (gdata (let ((fen-rdata (string->rdata gdata-fen)))
             (append
              (list
               (vector
                (logior +gdata-type+ (if (eq gdata-color 'white) #b00010000 0))
                0 0
                (length fen-rdata)))
              fen-rdata)))
    (t
     (error "unsupported type for make-server-packet ~a" type))))

(defun make-client-packet (type &key (hii-nickname "") move-x1 move-y1 move-x2 move-y2)
  (case type
    (hii (let ((nl-packets (string->rdata hii-nickname)))
           (append
            `(,(vector
                (logior (ash +hii-type+ 3) (logand (length nl-packets) #b11111000))
                (ash (logand (length nl-packets) #b111) 5)
                0
                0))
            nl-packets)))
    (move `(,(vector (logior +move-type+ (logand #xf move-x1))
                     (logior (ash (logand #xf move-y1) 4) (logand #xf move-x2))
                     (ash (logand #xf move-y2) 4)
                     0)))
    (t
     (error "unsupported type for make-client-packet ~a" type))))

(defun write-packet (conn packet)
  ;; (format t "will write packet ~a to ~a~%" packet conn)
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
  (if (usocket:wait-for-input conn :timeout 0.01)
      (let ((seq (make-sequence 'vector 4)))
        (read-sequence seq (usocket:socket-stream conn))
        seq)
      nil))

(defun write-packets (conn packets)
  (format t "will write packets: ~a~%" packets)
  (dolist (p packets)
    (write-packet conn p)))

(defun receive-packets (conn n)
  (loop for i from 1 to n collect (receive-packet conn)))

(defun rdata-packet->string (p)
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
  (if (packet-of-type-p p +move-type+)
      (values
       (logand (aref p 0) #xf)
       (ash (logand (aref p 1) #xf0) -4)
       (logand (aref p 1) #x0f)
       (ash (logand (aref p 2) #xf0) -4))
      (error "expected MOVE packet, got ~a instead" p)))

(defun start-p2p-server (game-handler)
  (format t "[SERVER] starting p2p server @ port ~a~%" +port+)
  (with-socket-listener
      (sock "127.0.0.1" +port+ :reuseaddress t :element-type '(unsigned-byte 8))
    ;; We're p2ping so accept 1 connection only
    (with-server-socket
        (conn (usocket:socket-accept sock))

      (write-packets conn (make-server-packet 'hii :hii-p2p t))

      (let* ((hii-back (receive-packet conn)))
        (assert (packet-of-type-p hii-back +hii-type+)))

      (write-packets conn (make-server-packet 'gdata :gdata-color 'white :gdata-fen +initial-fen+))

      (funcall game-handler conn)
      (format t "[SERVER] Closing p2p socket and connection~%"))))
      ;; (usocket:socket-close conn)
      ;; (usocket:socket-close sock))))

(defun start-server (game-handler &key (mode 'p2p) (fork nil))
  (declare (ignore fork))
  (case mode
    (p2p (start-p2p-server game-handler))
    (t (error "Unknown server mode `~a'" mode))))

(defun p2p-connect-and-return-fen-and-side-data (conn)
  (let* ((gdata (receive-packet conn))
         (side (if (= 0 (logand #b00010000 (aref gdata 0))) 'black 'white)))
    (values
     side
     (rdata-packets->string (receive-packets conn (aref gdata 3)))
     conn)))

(defun connect-to-server (ip nickname)
  (let ((conn (socket-connect ip +port+ :element-type '(unsigned-byte 8))))
    (let ((hii (receive-packet conn)))
      (format t "[CLIENT] got hii: ~a~%" hii)
      (if (logand (aref hii 0) #b00010000)
          (progn
            (write-packets conn (make-client-packet 'hii)) ;; we're in p2p land, don't use a nick
            (p2p-connect-and-return-fen-and-side-data conn))
          (error "non-p2p servers unsupported")))))
    ;; (write-packets conn (make-client-packet 'hii :hii-nickname nickname)))

(defun test ()
  (sb-thread:make-thread #'start-p2p-server)
  (connect-to-server "localhost" "hello hi :3")
  nil)
