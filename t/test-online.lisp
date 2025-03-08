(in-package :leszcz)

(thread
    "master"
  (net:connect-to-server
   "localhost"
   "hello world :3"
   :online-handler #'(lambda (conn)
                       (format t "conn: ~a~%" conn)
                       (net:write-packets conn (net:make-client-packet 'lgames))
                       (universal-start-server
                        conn
                        #'(lambda (fen side conn time)
                            (let ((g (fen->game fen)))
                              (initialize-game g 'white conn)
                              (format t "in game-handler w/ ~a ~a ~a ~a~%" fen side conn time)
                              (loop do
                                (maybe-receive-something g))))
                          :fen +initial-fen+
                          :opponent-side 'black
                          :time 13))))

(sleep 1)

(net:connect-to-server
 "localhost"
 "sigmator3000"
 :online-handler
 #'(lambda (conn)
     (format t "conn: ~a~%" conn)
     (net:write-packets conn (net:make-client-packet 'pgame :pgame-nick "hello world :3"))

     (let-values ((fen side _ time (p2p-connect-and-return-fen-and-side-data conn)))
       (let ((g (fen->game fen)))
         (initialize-game g 'white conn)
         (format t "in game-handler w/ ~a ~a ~a ~a~%" fen side conn time)
         (send-ping-to g)
         (loop do
           (maybe-receive-something g))))))
