# Co i Jak (dla "deweloperów")

## Co ("środowisko uruchomieniowe")
* win64 - skompilowałem te biblioteki dla *ciebie* czytelniku \emoji{purple-heart}
  * [raylib](https://pub.krzysckh.org/msc25/raylib5.5.dll)
  * [libffi](https://pub.krzysckh.org/msc25/libffi-8.dll)
* unix
  * raylib z taga 5.5 skompilowany z wsparciem dla PNG i `RAYLIB_LIBTYPE=LIBTYPE_SHARED`
    skopiowane do `$(PWD)/raylib5.5.so`
  * libffi typu jakiekolwiek
  
## Jak ("uruchamianie środowiska")
  
hmm, preferowalnie emacs z slime a potem `M-x slime RET`
```lisp
CL-USER> (push (pathname (uiop:getcwd)) ql:*local-project-directories*)
[...]
CL-USER> (ql:quickload :leszcz)
[...]
CL-USER> (in-package :leszcz)
#<PACKAGE "LESZCZ">
LESZCZ> (sb-thread:make-thread #'main)
[...]
```

Jeśli repl nie jest czymś co Ci się podoba, można wykonać `make run` by automatycznie i łatwo uruchomić kod.

:3

## bitboardy 

Na początku zimplementowałem szachy w sposób *mega* obiektowy, t.j. każdy `piece` ma swój `point`; gra ma *n* `pieces` i sprawiło to,
że kod działał ale był wybitnie wolny. Dlatego jako łatkę na ten spory problem użyłem bitboardów [@bitb] (tak jakby).
plansza w grze (`game`) jest odwzorowywana jako `fast-board` (2 x `fast-board-1`) - bitboardowa reprezentacja tego samego.
Sporo generacji ruchów opiera się właśnie na bitboardach przez co udało się *koda* lekko przyspieszyć.

Piony i króle cały czas generowane są przez wolniejszy ``algorytm" ale reszta figur sporo na tym zyskała.

## bot

Przy grze z botem (`bot.lisp`, `leszcz.lisp`) odpalany jest w nowym wątku serwer (domyślnie na porcie `net:+port+` = 3317) który przez
[protokół](#protokół) dogaduje się z głównym (interaktywnym) wątkiem symulując grę p2p.

## książka debiutów

Zaimplementowałem reader dla plików w formacie [polyglot](http://hgm.nubati.net/book_format.html) (`polyglot.lisp`), przez co leszcz może korzystać
ze sporej ilości standardowych książek. Domyślnie używam 11-letniej [książki](https://github.com/michaeldv/donna_opening_books/) silnika komodo używanej też w silniku donna.

## grafika

Funkcje które przejmują mainloop (np. żeby pokazać jakieś menu wyboru / menu główne) często korzystają z makra `with-continued-mainloop [cont-sym &body b]`
z funkcjami rysującymi i callbackami w &body. `cont-sym` to symbol ze zmienną która (np. po kliknięciu jakiegoś przycisku) będzie zawierać *kontynuację*, czyli funkcję,
która będzie kontynuować wykonywanie programu (nie wiem jak dobrze CL radzi sobie z usuwaniem tail calli, ale jeśli zacznie to przeszkadzać to po prostu powiększę maksymalny rozmiar call stacku :3).

np. funkcja która pokazuje interaktywnie błąd złapany przez najwyższy `handler-case` (gdy `*prod* != nil`) (na dzień 26/02/2025) wygląda następująco:

```lisp
(defun show-exception-interactively-and-continue (e)
  (let-values ((mesg (format nil "An unexcpected error has occurred: ~a~%" e))
               (btn w1 h1 (gui:make-button*
                           "Ok"
                           :height 24
                           :font-data alagard-data
                           :font-hash raylib::*alagard*
                           :text-draw-fn #'draw-text-alagard)))
    (with-continued-mainloop continuation
      (draw-text mesg 10 10 24 +color-white+)
      (funcall
       btn
       (/ *window-width* 2)
       (/ *window-height* 2)
       #'(lambda (_)
           (declare (ignore _))
           (setf continuation #'(lambda ()
                                  (cleanup-threads!)
                                  (main))))))))
```

jest to (moim zdaniem) dość konkretny i zwięzły sposób na napisanie funkcji z menu.

## cytaty

\epigraph{jezu chryste p \textbf{(6 6)} to piece(PAWN[BLACK])@\textbf{point(5 7)}}{--- \textup{pre--possible-moves-for/upgrade}, leszcz.lisp}
\epigraph{point-checked-p disagree on point (4, 0) w/ fen r3k2r/p1pp1qbn/bn2p1p1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K1R1 b Qkq - 0 1 being checked by WHITE (NIL vs T)}{--- \textup{point-checked-p}, leszcz.lisp}
\epigraph{``\emoji{cross-mark} 97818 is expected to be 97862"}{--- t/test.lisp}
\epigraph{``safe piece type of point (7 7) is NIL in contrary to the unsafe one which is ROOK"}{--- \textup{maybe-castling-moves}, leszcz.lisp}
\epigraph{``King couldn't be found in \#S(FAST-BOARD-1 ...)"}{--- fb1-king-of, fast.lisp}


