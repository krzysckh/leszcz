* [ ] ПРЕЛЮДИЯ
  * [ ] i18n
  * [ ] usun przyciski w grze offline
  * [ ] grafika
    * [x] takie wihajstry z opisami przycisków co się pojawiają jak najeżdżasz
    * [x] podświetl szachy
    * [ ] nie pozwól ciągowi kontynuacji się zawalić, jeśli window-close-p po prostu zamknij program jakimś (exit) idk
    * [x] plansza typu dwukolorowa
    * [ ] ładniejsze figury
      * [x] ładniejsze figury I
      * [x] ładniejsze figury II (nałóż filtry poprawnie)
      * [x] ładniejsze figury III (graficzki od maciusia)
        * [x] pion
        * [x] wieża
        * [x] koń
        * [x] goniec
        * [ ] król
          * [x] jest
          * [ ] no ale mógłby być ładniejszy
        * [ ] hetman
          * [x] jest
          * [ ] no ale mogłaby być ładniejsza
      * [x] możliwość wybrania figur i kolorów
    * [x] menu
    * [ ] opcje
  * [x] "gameplay"
    * [x] remis
    * [x] cofnięcie ruchu
    * [x] tury
    * [x] zbijanie figur
    * [x] generowanie i podświetlanie możliwych ruchów
      * [x] pion
        * [x] ruchy typu o 1
        * [x] ruchy typu o 2
        * [x] bicie
        * [x] zmiana w hetmana
        * [x] zmiana w cokolwiek innego + wykmiń generację ruchów + menu dla gracza
        * [x] ę pasą (bicie w przelocie)
      * [x] wieża
      * [x] goniec
      * [x] skoczek
      * [x] hetman
      * [x] król
      * [x] szachy (nawiązanie do gry szachy)
        * [x] szachy prawie wszystkie
        * [x] patrz roszada -> szach
      * [x] maty
      * [x] paty
    * [x] :3 meow meow meow meow
      * [x] meow meow meow meow 1
      * [x] meow meow meow meow 2
      * [x] meow meow meow meow 3
      * [x] meow meow meow meow 4
      * [x] meow meow meow meow 5
      * [x] meow meow meow meow 6
      * [ ] meow meow meow meow 7
    * [x] roszada
      * [x] roszada typu roszada
      * [x] szach !!!!! !!! ! ! ! 
    * [x] szachmat
  * [x] "bot"
    * [x] AAAAAAAAAAA !!!!!!
    * [x] bot typu losowy
    * [x] przyspiesz generacje ruchów
      * [x] może jest okej ale cache trzeba trzymać jako bitboard + eval + ???
    * [x] dodaj jakiś eval
    * [x] "zobrist hashing"
  * [ ] jeśli ci się będzie nudzić
    * [ ] [asdf:test-system](https://github.com/fukamachi/prove?tab=readme-ov-file#asdf-integration) zamiast make test
    * [ ] grep -R TODO
* [x] Gracz v Gracz na jednym komputerze
  * [x] ej bo to juz w sumie jest
    * [x] kłamałem
      * [x] ja pierdole
        * [x] już już jest
  * [x] wybór w menu (parz preludija → menu)
  * [x] timer
    * [x] jest
    * [x] działa
  * [ ] edytor pozycji
  * [ ] import FEN
  * [ ] odtwarzanie gry
    * [ ] "Gra musi zapisywać historię ruchów w notacji algebraiczne"
      * [x] > Gra musi zapisywać historię ruchów
      * [ ] > w notacji algebraicznej
        * [x] no większość ruchów
        * [ ] no mniejszość ruchów
      * [ ] i jeszcze zapisywać to potem może jakoś
  * [x] pokaż ostatni ruch zaznacz jakoś ładnie
  * [ ] efekty dźwiękowe
    * [ ] preferowalnie nie paszczowe
    * [ ] preferowalnie nie takie za które pójdę za kratki (CC0)
* [ ] Gracz kontra komputer
  * [x] obejrzyj filmik sebusia i zerżnij pomysły (wszystkie)
    * [x] minmax
    * [x] "jak ruch slabszy niz wczescniej widziany to juz nie licz dalej" = redukcja alpha-beta
    * [x] wyszukiwanie w drzewie sposobem monte carlo
      * [x] to nie działa pozdr
    * [x] preferencja figur dla danych pozycji (`*bonus-table*`)
  * [x] a tak w ogóle to silnik miej w osobnym wątku żeby mniej się narobić a więcej zarobić
  * [ ] bot nie może apgrejdować piona XDDD
    * [x] może ale nie może wybrać na co XDDDD
  * [ ] książka
    * [x] impl formatu polyglot
    * [ ] napisz na krz org artykuł o implementowaniu tego w cl bo to ciekawe
          i źródło to http://hgm.nubati.net/book_format.html
    * [x] jakakolwiek książka
    * [x] lepsza książka
    * [x] kilka książek z debiutami różnych GMów i możliwość wyboru
* [ ] Gracz v Gracz na różnych komputerach w LAN (p2p)
  * [x] działa
  * [x] jakiekolwiek menu do hostowania/łączenia
    * [x] do hostowania
    * [x] do łączenia
  * [ ] ładne menu do hostowania/łączenia
    * [ ] do hostowania
    * [ ] do łączenia
* [ ] Gracz v Gracz na różnych komputerach w sieci internet (symulowane p2p)
  * [x] działa
  * [ ] serwer powinien nie pozwalać połączyć się n graczom o takim samym nicku
  * [ ] lepsze rozłączanie starych fd
  * [x] jakiekolwiek menu do hostowania/łączenia
    * [x] do hostowania
    * [x] do łączenia
  * [ ] ładne menu do hostowania/łączenia
    * [ ] do hostowania
    * [ ] do łączenia
* [x] protokół
  * [x] napisane docsy w całości
  * [x] impl.
    * [x] hii
    * [x] gdata
    * [x] lgames
    * [x] pgame
    * [x] ping
    * [x] move
      * [x] dane o ruchu
      * [x] dane o upgrejdzie piona !!!!!
    * [x] rdata
* [x] napisz resztę todo
  * [x] pozdro
  * [x] pozdro 2
  * [x] pozdro 3
