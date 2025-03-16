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
że kod działał ale był wybitnie wolny. Dlatego jako łatkę na ten spory problem użyłem [bitboardów](https://www.chessprogramming.org/Bitboards#General_Bitboard_Techniques) (tak jakby).
plansza w grze (`game`) jest odwzorowywana jako `fast-board` (2 x `fast-board-1`) - bitboardowa reprezentacja tego samego.
Sporo generacji ruchów opiera się właśnie na bitboardach przez co udało się *koda* lekko przyspieszyć.

Piony i króle cały czas generowane są przez wolniejszy ``algorytm" ale reszta figur sporo na tym zyskała.

## bot

Przy grze z botem (`bot.lisp`, `leszcz.lisp`) odpalany jest w nowym wątku serwer (domyślnie na porcie `net:+port+` = 3317) który przez
[protokół](#protokół) dogaduje się z głównym (interaktywnym) wątkiem symulując grę p2p.

## książka debiutów

Zaimplementowałem reader dla plików w formacie [polyglot](http://hgm.nubati.net/book_format.html) (`book.lisp`), przez co leszcz może korzystać
ze sporej ilości standardowych książek. Domyślnie używam 11-letniej [książki](https://github.com/michaeldv/donna_opening_books/) silnika komodo używanej też w silniku donna.

## książka debiutów arcymistrzów

Żadnych polyglotowych książek debiutów arcymistrzów nie znalazłem, więc musiałem zrobić swoje.
Nie trzymam ich w formacie polyglot, tylko tym co wypluwa `cl-store`, żeby móc trzymać więcej informacji niż tylko najlepszy ruch.
Pozwala to na trzymanie m.in danych o grze z której pochodzi ruch, czy **N** ruchów zamiast jednego, co wprowadza wariację w tym jak grają boty arcymistrzów.

Raz przez to, że nie ustawiłem ziarna generatora losowych liczb wpadłem na buga przez którego na `1.d4` bot Hikaru zawsze odpowiadał `h6` i po kontynuacji `2.c4` odpowiadał `g6`.
Po przeglądnięciu bazy danych dokopałem się, że była to gra z arcymistrzem Danielem Naroditskym (!), którą zremisował (!!).

\begin{figure}[H]
  \centering
  \ttfamily
  \newchessgame
  \hidemoves{1.d4 h6 2.c4 g6}
  \chessboard[showmover=false]
  \rmfamily
\end{figure}

Wracając do info technicznego, troche faux pas z trzymaniem dużych plików w gicie no ale generowanie tych plików .dat jest
trochę powolne. Mógłbym zoptymalizować mój reader pgnów, ale to wymagałoby czasu, a cache-owanie plików binarnych jest darmowe :P.

Nie wiem też czemu tak wybitnie powiększyło to rozmiar budowanego .exe dla windowsa...?
Sumaryczny rozmiar tych baz `.dat` to tylko `18M` a powiększa finalny plik wykonywalny o `210M`! No cóż, może to kiedyś naprawię :/.

Ecl jest w stanie zbudować 15-megabajtowy plik wykonywalny, ale na 100% ładuje coś podczas wykonywania...

## grafika

Funkcje które przejmują mainloop (np. żeby pokazać jakieś menu wyboru / menu główne) często korzystają z makra `with-continued-mainloop [cont-sym &body b]`
z funkcjami rysującymi i callbackami w &body. `cont-sym` to symbol ze zmienną która (np. po kliknięciu jakiegoś przycisku) będzie zawierać *kontynuację*, czyli funkcję,
która będzie kontynuować wykonywanie programu (nie wiem jak dobrze CL radzi sobie z usuwaniem tail calli, ale jeśli zacznie to przeszkadzać to po prostu powiększę maksymalny rozmiar call stacku :3).

np. funkcja która pokazuje interaktywnie błąd złapany przez najwyższy `handler-case` (gdy `*prod* != nil`) (na dzień 26/02/2025) wygląda następująco:

```lisp
(defun show-exception-interactively-and-continue (e)
  (let-values ((mesg (format nil "An unexcpected error has occurred: ~%~a~%" e))
               (btn w1 h1 (abtn "Ok" :height 24)))
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

## testowanie

[Testy](https://github.com/fukamachi/prove) są w folderze `t/`. uruchamiane przez `make test`, `make test-p2p`, `make test-online`, `make test-online-2`.
Sprawdzają generatory ruchów, konstruktory pakietów i działanie przez sieć.

## CI

Napisałem CI dla github actions żeby sprawdzić czy każdy commit może być poprawnie zbudowany do pliku wykonywalnego (`.github/workflows/ci.yml`).

## cytaty

\epigraph{jezu chryste p \textbf{(6 6)} to piece(PAWN[BLACK])@\textbf{point(5 7)}}{--- \textup{pre--possible-moves-for/upgrade}, leszcz.lisp}
\epigraph{point-checked-p disagree on point (4, 0) w/ fen r3k2r/p1pp1qbn/bn2p1p1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K1R1 b Qkq - 0 1 being checked by WHITE (NIL vs T)}{--- \textup{point-checked-p}, leszcz.lisp}
\epigraph{``\emoji{cross-mark} 97818 is expected to be 97862"}{--- t/test.lisp}
\epigraph{``safe piece type of point (7 7) is NIL in contrary to the unsafe one which is ROOK"}{--- \textup{maybe-castling-moves}, leszcz.lisp}
\epigraph{``King couldn't be found in \#S(FAST-BOARD-1 ...)"}{--- fb1-king-of, fast.lisp}
\epigraph{;; Steal mainloop and show a menu that can configure data from :leszcz-constants (not so constant now huh?)}{--- gui.lisp}

```lisp
(defmacro for-every-bb (as n &body b)
  ;; n to tak naprawdę fb tylko dużo zabawniejszy jest let pacan
  (append                                        ;          |
   '(progn)                                      ;          |
   (apply                                        ;          |
    #'append                                     ;          |
    (loop                                        ;          |
      for ca in '(fb-white fb-black)             ;          |
      collect (loop for pa in '(fb-pawn fb-rook fb-knight fb-bishop fb-queen fb-king)
                    collect                      ;          |
                    `(let ((,as (,pa (,ca ,n)))) ; <- tu o -+
                       ,@b                       ;
                       (setf (,pa (,ca ,n)) ,as) ; a tu to nawet pacanas!
                       ))))))
```
