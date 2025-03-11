# Architektura

Aplikacja napisana jest w Common Lispie i korzysta z implementacji [sbcl](https://sbcl.org).
Do obsługi międzyplatformowej grafiki/IO korzysta z bliblioteki [raylib](https://raylib.com).
Napisałem ad-hoc nakładkę korzystającą z [cffi](https://cffi.common-lisp.dev/)
i [libffi](https://www.chiark.greenend.org.uk/doc/libffi-dev/html/) która ładuje dll
rayliba i binduje funkcje do takich łatwo używalnych z lispa.

Dzięki takiemu podejściu aplikacja jest dość międzyplatformowa[^1] i jeśli pominie się
budowanie pliku wykonywalnego (albo użyje [buildapp](https://www.xach.com/lisp/buildapp)) może[^2]
być uruchamiana innych implementacjach CL.

Serwer, który pozwala na grę online (nie tylko po LAN) napisany jest w języku [Owl Lisp](https://gitlab.com/owl-lisp/owl), bo:

- taki miałem kaprys 
- nie chciałem mieć na serwerze uruchomionego common lispowego giganta
- lubię multitasking w owl lispie - zielone wątki zamiast prawdziwych procesów-dzieci


wymagane do uruchomienia .exe:

- win10 x64
- wsparcie dla opengl (na hw powinno działać poprawnie od buta, [mesa](https://github.com/pal1000/mesa-dist-win/releases) też git)

[^1]: przynajmniej MS Windows (x64), Debian GNU/Linux, OpenBSD.
[^2]: sprawdziłem, działa przynajmniej w ECL i CCL
