# Sieć


(to jest pomysł, jeszcze nie napisałem ani linijki kodu :3)

## p2p

1v1 po lokalnej sieci LUB sieci internet przy otwartych portach

- Komputer 1 (`master`) uruchamia serwer na jakimśtam porcie gotowy do akceptowania 1 połącznia
- Komputer 2 (`slave`) łączy się wprost z komputerem 1
- dogadują się przez jakiśtam protokół
- ...
- profit

## przez sieć internet

- Komputer 1 łączy się z jakimśtam (pewnie moim) serwerem po adresie IP w sieci internet
- Komputer 2 łączy się z tym samym serwerem
- dogadują się przez jakiśtam protokół
- dogadują się **z kim** chcą grać (po nazwie użytkownika? po adresie ip? :3)
- serwer (ten jakiśtam [pewnie mój]) przekazuje dane z Komputera 1 do Komputera 2 bez łączenia ich bezpośrednio ze sobą
- ...
- profit

# protokół

## typy pakietów

| typ | nazwa  | opis                                                                      |
|-----|--------|---------------------------------------------------------------------------|
| 0   | hii :3 | wysyłany na początku by dostać informacje o typie serwera i możliwościach |
| 1   | gdata  | informacje o grze przed rozpoczęciem/podczas                              |
| 2   | lgames | prośba/wylistowanie możliwych gier przez serwer                           |
| 3   | pgame  | wybierz grę i dołącz                                                      |
| 4   | ping   | ping, czy komputer jest nadal aktywny?                                    |
| 5   | move   | ruch                                                                      |
| 6   | rdata  | dane wysyłane jako kontynuacja poprzedniego pakietu                       |

## opisy pakietów

TODO: każda gra musi mieć ID i musi być ich mniej niż 2^29

* *hii :3*
  * serwer wysyła
\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{29}{konfiguracja} \\
    \bitboxes{1}{000} & \bitbox{1}{\rotatebox{90}{p2p?}} & \bitbox{28}{zarezerwowane}
  \end{bytefield}
\end{figure}
  * klient odpowiada
\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{8}{nicklen} & \bitbox{21}{zarezerwowane} \\
    \bitboxes{1}{000} & \bitbox{8}{0 jeśli p2p} & \bitbox{21}{zarezerwowane} \\
  \end{bytefield}
\end{figure}

TODO: nicklen w ilości pakietów? łoł czyli 0-`3*255`

i *nicklen* pakietami kontynuacyjnymi

\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{5}{} & \bitbox{24}{kolejne znaki nicku} \\
    \bitboxes{1}{110} & \bitbox{1}{\rotatebox{90}{Kontynuacja?}} & \bitbox{4}{\rotatebox{90}{zarezerwowane}} &
    \bitbox{8}{chr1} & \bitbox{8}{chr2} & \bitbox{8}{chr3}
  \end{bytefield}
\end{figure}

* *gdata*
  * serwer wysyła
\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{29}{dane} \\
    \bitboxes{1}{001} & \bitbox{1}{\rotatebox{90}{biały?}} & \bitbox{20}{zarezerwowane} & \bitbox{8}{ilość rdata kont.} \\
  \end{bytefield}
\end{figure}

i *N* pakietów kontynuacyjnych zawierających FEN gry

* *lgames*
  * klient wysyła
\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{29}{maksymalna ilość odebranych gier} \\
    \bitboxes{1}{010} & \bitbox{29}{j.w.} \\
  \end{bytefield}
\end{figure}
  * serwer odpowiada

\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{29}{ilość pakietów kontynuacyjnych} \\
    \bitboxes{1}{010} & \bitbox{29}{j.w.} \\
  \end{bytefield}
\end{figure}

  i *N* pakietami kontynuacyjnymi

\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{5}{} & \bitbox{24}{znaki nazwy gry (jeśli nie 0)} \\
    \bitboxes{1}{110} & \bitbox{1}{\rotatebox{90}{Kontynuacja?}} & \bitbox{4}{\rotatebox{90}{zarezerwowane}} &
    \bitbox{8}{chr1} & \bitbox{8}{chr2} & \bitbox{8}{chr3}
  \end{bytefield}
\end{figure}

* *move*

;; TODO: protocol HAS to include data about the chosen piece after upgrade

\begin{figure}[H]
  \centering
  \ttfamily
  \begin{bytefield}{32}
    \bitheader{0-31} \\
    \bitbox{3}{typ} & \bitbox{1}{} & \bitbox{28}{dane o ruszeniu bierki} \\
    \bitboxes{1}{101} & \bitbox{1}{\rotatebox{90}{nieużywane}} & \bitbox{4}{x1} & \bitbox{4}{y1} & \bitbox{4}{x2} & \bitbox{4}{y2} & \bitbox{12}{zarezerwowane} \\
  \end{bytefield}
\end{figure}

## Przykłady 

### p2p

\begin{figure}[H]
  \centering
  \ttfamily
  \begin{sequencediagram}
    \newinst{a}{Master}{}
    \newinst[1]{b}{Slave}{}
    \begin {sdblock}{"Handshake"}{}
      \mess[1]{a}{hii :33 [p2p?=1]}{b}
      \mess[1]{b}{hii :33}{a}
    \end{sdblock}
    \begin {sdblock}{Dołączenie do gry}{}
      \mess[1]{a}{gdata}{b}
      \mess[1]{a}{rdata z fenem}{b}
      \mess[1]{a}{rdata [kont] ...}{b}
    \end{sdblock}
    \begin {sdblock}{Gra}{}
      \mess[1]{a}{move}{b}
      \mess[1]{b}{move}{a}
      \mess[1]{a}{move}{b}
      \mess[1]{b}{move}{a}
    \end{sdblock}
  \end{sequencediagram}
\end{figure}
