* [ ] ПРЕЛЮДИЯ
  * [ ] grafika
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
      * [ ] możliwość wybrania figur i kolorów
    * [ ] menu
    * [ ] opcje
  * [x] "gameplay"
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
      * [x] meow meow meow meow meow meow 1
      * [x] meow meow meow meow meow meow 2
      * [x] meow meow meow meow meow meow 3
      * [x] meow meow meow meow meow meow 4
      * [ ] meow meow meow meow meow meow 5
      * [ ] meow meow meow meow meow meow 6
      * [ ] meow meow meow meow meow meow 7
    * [x] roszada
      * [x] roszada typu roszada
      * [x] szach !!!!! !!! ! ! ! 
    * [x] szachmat
  * [ ] "bot"
    * [x] AAAAAAAAAAA !!!!!!
    * [x] bot typu losowy
    * [x] przyspiesz generacje ruchów
      * [x] może jest okej ale cache trzeba trzymać jako bitboard + eval + ???
    * [x] dodaj jakiś eval
  * [ ] jeśli ci się będzie nudzić
    * [ ] [asdf:test-system](https://github.com/fukamachi/prove?tab=readme-ov-file#asdf-integration) zamiast make test
    * [ ] grep -R TODO
* [ ] Gracz v Gracz na jednym komputerze
  * [x] ej bo to juz w sumie jest
    * [x] kłamałem
      * [x] ja pierdole
  * [ ] wybór w menu (parz preludija → menu)
  * [ ] timer
  * [ ] edytor pozycji
  * [ ] import FEN
  * [ ] odtwarzanie gry
    * [ ] "Gra musi zapisywać historię ruchów w notacji algebraiczne"
      * [x] > Gra musi zapisywać historię ruchów
      * [ ] > w notacji algebraicznej
  * [x] pokaż ostatni ruch zaznacz jakoś ładnie
  * [ ] efekty dźwiękowe
    * [ ] preferowalnie nie paszczowe
    * [ ] preferowalnie nie takie za które pójdę za kratki (CC0)
* [ ] Gracz kontra komputer
  * [x] obejrzyj filmik sebusia i zerżnij pomysły (wszystkie)
  * [x] nazwij algorytmy tak żeby brzmiało mądrze
    * [x] "odcinanie chujowych ruchów" → minmax
    * [x] "mądre obcinanie chujowych ruchów" = "jak ruch slabszy niz wczescniej widziany to juz nie licz dalej" → redukcja alpha-beta
    * [x] "wybieranie ruchów z listy możliwych losowo zamiast liniowi" → wyszukiwanie w drzewie sposobem monte carlo
      * [x] to nie działa
    * [x] preferencja figur dla danych pozycji (`*bonus-table*`)
  * [x] a tak w ogóle to silnik miej w osobnym wątku żeby mniej się narobić a więcej zarobić
  * [x] bot nie może apgrejdować piona XDDD
    * [ ] może ale nie może wybrać na co XDDDD
* [ ] Gracz v Gracz na różnych komputerach w LAN (p2p)
  * [x] działa
  * [ ] jakiekolwiek menu do hostowania/łączenia
  * [ ] ładne menu do hostowania/łączenia
* [ ] protokół
  * [ ] napisane docsy w całości
  * [ ] impl.
    * [x] hii
    * [ ] gdata
    * [ ] lgames
    * [ ] pgame
    * [ ] ping
    * [x] move
      * [x] dane o ruchu
      * [x] dane o upgrejdzie piona !!!!!
    * [x] rdata
* [ ] napisz resztę todo
  * [x] pozdro
  * [x] pozdro 2
