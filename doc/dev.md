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

## bitboardy [@bitb]

```lisp
(setf (game-fb ...) (game->fast-board ...))
```

Generacja ruchów niektórych figur opiera się na bitplanszach `fast-board`, `fast-board-1` w pliku `fast.lisp`.

## cytaty

\epigraph{jezu chryste p \textbf{(6 6)} to piece(PAWN[BLACK])@\textbf{point(5 7)}}{--- \textup{pre--possible-moves-for/upgrade}, leszcz.lisp}
\epigraph{point-checked-p disagree on point (4, 0) w/ fen r3k2r/p1pp1qbn/bn2p1p1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K1R1 b Qkq - 0 1 being checked by WHITE (NIL vs T)}{--- \textup{point-checked-p}, leszcz.lisp}
\epigraph{``\emoji{cross-mark} 97818 is expected to be 97862"}{--- t/test.lisp}
\epigraph{``safe piece type of point (7 7) is NIL in contrary to the unsafe one which is ROOK"}{--- \textup{maybe-castling-moves}, leszcz.lisp}
\epigraph{``King couldn't be found in #S(FAST-BOARD-1 ...)"}{--- fb1-king-of, fast.lisp}


