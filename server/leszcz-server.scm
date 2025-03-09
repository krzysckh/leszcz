;; -*- Owl -*-

(import
 (owl toplevel)
 (owl thread)
 (prefix (owl sys) sys/))

(define *port* 3317)

(define bvref bytevector-u8-ref)

;;; TODO
;; - [x] Accept a connection from a client
;; - [x] the client can become a game host
;; - [x] the client can become a game slave
;; - [x] the client can get a list of games
;; - [x] the client can join a game -- start a new thread that literally just sends messages between the 2 clients.
;; - [ ] after a finished game disconnect both clients - close ports !!!!
;; - [ ] handle errors on closed connections and don't literally just fail™

(define type-hii    #b000)
(define type-gdata  #b001)
(define type-lgames #b010)
(define type-pgame  #b011)
(define type-ping   #b100)
(define type-move   #b101)
(define type-rdata  #b110)

(define (get-packets fd n)
  ;; (when-readable fd)
  (map (λ (_) (try-get-block fd 4 #t)) (iota 0 1 n)))

;; yes i know i could have just used (owl variable), but i wanted to have "le full control"
(define (start-game-data-handler)
  (thread
   'game-data-handler
   (let loop ((lst #n))
     (lets ((who v (next-mail)))
       (tuple-case v
         ((add! thing) ; thing is (id rest ...)
          (print "[GDH] add! " thing)
          (loop (append lst (list thing))))
         ((delete! id)
          (print "[GDH] delete! " id)
          (loop (filter (λ (x) (not (equal? (car x) id))) lst)))
         ((get! id)
          (print "[GDH] get! " id)
          (mail who (cdr* (assoc id lst equal?)))
          (loop lst))
         ((list!)
          (print "[GDH] list!")
          (mail who lst)
          (loop lst))
         (else
          (loop lst)))))))

(define (get-game! id)    (interact 'game-data-handler (tuple 'get! id)))
(define (add-game! thing) (mail 'game-data-handler (tuple 'add! thing)))
(define (delete-game! id) (mail 'game-data-handler (tuple 'delete! id)))
(define (list-games!)     (interact 'game-data-handler (tuple 'list!)))

(define (maybe-next-mail)
  (let ((envelope (check-mail)))
    (if (tuple? envelope)
        (values (ref envelope 1) (ref envelope 2))
        (values #f #f))))

;; lmao
(define (cdddr* l)
  (cdr* (cdr* (cdr* l))))

(define (maybe-lref l n)
  (if (<= (length l) n) 0 (lref l n)))

(define (string->rdata s)
  (let loop ((l (string->list s)) (cont? #f))
    (if (null? l)
        #n
        (append
         `((,(bior (<< type-rdata 5) (if cont? #b00010000 0))
            ,(maybe-lref l 0)
            ,(maybe-lref l 1)
            ,(maybe-lref l 2)))
         (loop (cdddr* l) #t)))))

(define (list->rdatas lst)
  (map string->rdata lst))

(define (rdatas->string packets)
  (list->string
   (fold
    (λ (a bv) (append a (filter
                         (λ (x) (not (= 0 x)))
                         (list (bvref bv 1) (bvref bv 2) (bvref bv 3)))))
    #n
    packets)))

(define (loud-error . l)
  (let ((e (fold str "" l)))
    (print-to stderr "loud-error: " e)
    (error "loud-error: " e)))

;; packet is the 1st packet of hii :3
(define (handle-introduction packet fd)
  (let* ((cont (bior (<< (band #b00011111 (bvref packet 0)) 3) (>> (band #b11100000 (bvref packet 1)) 5)))
         (packets (get-packets fd cont)))
    (rdatas->string packets)))

(define (bail-out! fd)
  (format stdout "bailing out for ~a!~%" fd)
  (try-thunk
   (λ ()
     (when (writeable? fd)
       (write-bytes fd '(#b00100000 #b00000001 0 0)))
     (print "fd is: " fd)
     (close-port fd))
   (λ (e) (print "bail-out: failed to write-bytes: " e))
   (string->symbol (str "_bailout" (time-ns)))))

(define (simulate-p2p slave g)
  (let ((master (lref g 0)))
    (format stdout "will simulate p2p for ~a and ~a with game ~a~%" g master slave)
    (map (H write-bytes slave) (lref g 1)) ; write the initial gdata config packet

    (format stdout "slave=~a, master=~a~%" slave master)

    (thread
     (let loop ()
       (sleep 20)
       (if (readable? master)
           (if-lets ((packet (car* (get-packets master 1)))
                     (_ (bytevector? packet))) ; <- a hackish thing but heh
             (begin
               (print "packet from master is " packet)
               (write-bytes slave (bytevector->list packet))
               (loop))
             (begin
               (bail-out! master)
               (bail-out! slave)))
           (loop))))

    (thread
     (let loop ()
       (sleep 20)
       (if (readable? slave)
           (if-lets ((packet (car* (get-packets slave 1)))
                     (_ (bytevector? packet)))
             (begin
               (print "packet from slave is " packet)
               (write-bytes master (bytevector->list packet))
               (loop))
             (begin
               (bail-out! slave)
               (bail-out! master)))
           (loop))))
    ))


(define (make-client fd ip)
  (let ((thrname (string->symbol (str "client@" ip "-" (time-ns)))))
    (thread
     thrname
     (begin
       (write-bytes fd '(0 0 0 0)) ;; hii :3 w/ p2p=FALSE
       (let loop ((uname "<USERNAME UNKNOWN>"))
         (if (readable? fd)
             (let* ((bv (try-get-block fd 4 #t)))
               ;; eq? instead of = bc (bytevector-length) on #f yields #f and = on #f yields an (loud-error)
               (if (eq? (bytevector-length bv) 4)
                   (let ((type (>> (band #b11100000 (bvref bv 0)) 5)))
                     (print "got message of type " type)
                     (case type
                       (type-hii
                        (let ((username (handle-introduction bv fd)))
                          (print "username: " username)
                          (loop username)))
                       (type-gdata
                        (print "got a type gdata")
                        (let* ((ncont (bvref bv 3))
                               (_ (print "ncont: " ncont))
                               (packets (append (list (bytevector->list bv))
                                                (map bytevector->list (get-packets fd ncont)))))
                          (print "packets: " packets)
                          (add-game! (list uname fd packets)) ; > immutable (yeah right!)
                          ;; We're cheating a bit as we're not actually holding the idividual gdata values
                          ;; and just the packet and fd to send it when simulating p2p mode
                          (print "Added a game! it's: " (get-game! uname))
                          ))
                          ;; (loop uname)))
                       (type-pgame ; pick game
                        (print "pgame!")
                        (let* ((ncont (bvref bv 3))
                               (nickname (rdatas->string (get-packets fd ncont))))
                          (if-lets ((g (get-game! nickname)))
                            (begin
                              (delete-game! nickname)
                              (simulate-p2p fd g))
                            (bail-out! fd)))) ; this does not (loop), we move the loop to simulate-p2p or bail out
                       (type-lgames
                        (let ((rdatas (fold append #n (list->rdatas (map car (list-games!))))))
                          (write-bytes fd (list
                                           (<< type-lgames 5)
                                           0
                                           (>> (band #xff00 (len rdatas)) 8)
                                           (band #xff (len rdatas))))
                          (for-each (H write-bytes fd) rdatas)
                          (loop uname)))
                       (else
                        (loud-error "invalid packet type: " type)
                        (loop uname))))
                   (begin
                     (print "4 != 4 bailing out (client probably disconnected)")
                     (bail-out! fd)
                     (close-port fd))))
             (loop uname)))))))
         ;; (if-lets ((_ v (maybe-next-mail)))
         ;;   (send-fasl fd v))

(λ (_)
  (let ((sock (open-socket *port*)))
    (format stdout "opened socket at port ~a~%" *port*)

    (start-game-data-handler)

    (thread
     'accepter
     (let loop ()
       (lets ((ip fd (tcp-client sock)))
         (make-client fd ip)
         (loop))))))
