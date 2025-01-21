# Co i Jak (dla "deweloperów")

## Co ("środowisko uruchomieniowe")
* win64 - skompilowałem te biblioteki dla *ciebie* czytelniku 💜
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

