# leszcz

Leszcz to program do gry w szachy i *mini bot* szachowy. Nazywam go *mini botem*
bo nie jest nazbyt mądry. Ma preferencje w debiutach, ma do dyspozycji książki (o tym więcej później),
ale przez sposób implementacji jest w stanie spojrzeć tylko na 3 ruchy naprzód i często się na to nacina
wykonując fajtłapiczne ruchy ponieważ nie jest w stanie policzyć, że po danych 3 ruchach przeciwnik coś może odbić.
Niestety na optymalizacje nie zostało czasu.

Nazwa leszcz nawiązuje do starego silnika szachowego *rybka*.

Duże modele językowe ("sztuczna inteligencja") nie były wykorzystywane w tym projekcie. Został on od początku do końca napisany przez człowieka.

## Tryby gry

- offline
  * gracz vs gracz na jednym komputerze: Uruchamiane jest okno z planszą. Plansza odwraca się w zależności od ruchu by pokazać się ze strony wykonującego ruch.
  * gracz vs bot: Normalna gra na bota.
  * gracz vs "arcymistrz": Normalna gra na bota nauczonego debiutów z bazy danych gier danego arcymistrza.
- online
  * gracz vs gracz przez LAN: jeden gracz wybiera opcję "hostuj", drugi "dołącz" (oba przez LAN) i pojedynek toczy się po sieci lokalnej
  * gracz vs gracz przez sieć internet: jeden gracz wybiera opcję "hostuj", drugi "dołącz" i drugi po nazwie użytkownika odnajduje pierwszego na liście
    gier, po czym dołącza i toczy się gra. Za hosting gier i przekazywanie informacji odpowiada mój serwer. Można postawić własny, o tym później.
    
## Jak grać

Przytrzymaj figurę i przesuń myszkę na dane pole by wykonać ruch. Figury można przesuwać tylko na podświetlone pola.
W przypadku zakończenia gry pokaże się na ekranie stosowny komunikat.

Podczas gry można przycisnąć na klawiaturze przycisk `N`, by włączyć/wyłączyć *tryb nerda* \emoji{nerd-face}.
