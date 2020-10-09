# Haskell

## Laivų mūšis. Žinutės struktūra

- Kiekviena ne pirma žinutė turi lauką `prev`, kuriame yra išsaugotos visos prie tai buvusios žinutės.
- Kiekviena ne pirma žinutė turi eilutės tipo lauką `result`, kuris nurodo prieš tai buvusio ėjimo rezultatą: "MISS" arba "HIT".
- Kiekviena žinutė turi sąrašo iš eilučių tipo lauką `coord`, kuriame nurodytos ėjimo koordinatės, pvz., `["A","5"]`. Paskutinės žinutės reikšmė yra `[]`.

Visi raktai yra be nematomų simbolių (t.y. be tarpų, tab'ų ir pan.).

## Pavyzdys

Žinutė (kažkokiu nežinomu bet pakankamai suprantomu formatu):

~~~
(coord -> ["A", "2"], result -> "MISS", prev -> (coord -> ["A", "1"], result -> "HIT", prev -> (coord -> ["B", "5"])))
~~~

atitinka žaidimo laukus (lentas):

~~~
Laukas A                    Laukas B
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
|  |A|B|C|D|E|F|G|H|I|J|    |  |A|B|C|D|E|F|G|H|I|J|
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 1|x| | | | | | | | | |    | 1| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 2| | | | | | | | | | |    | 2|?| | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 3| | | | | | | | | | |    | 3| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 4| | | | | | | | | | |    | 4| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 5| | | | | | | | | | |    | 5| |*| | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 6| | | | | | | | | | |    | 6| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 7| | | | | | | | | | |    | 7| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 8| | | | | | | | | | |    | 8| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
| 9| | | | | | | | | | |    | 9| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
|10| | | | | | | | | | |    |10| | | | | | | | | | |
+--+-+-+-+-+-+-+-+-+-+-+    +--+-+-+-+-+-+-+-+-+-+-+
~~~

## Žaidimo bendros taisyklės:
1. Laivai neliečia vieni kitų (net kampais)
2. Atsakymai į ėjimą yra tik 2 tipų: MISS ir HIT (taisyklė "sunk in silence")

## Protokolas be žodynų

Bet kurį žodyną visada galimą užrašyti kaip masyvą elementų, kuriame nelyginiai elementai yra raktai, o lyginiai -- reikšmės. Tada

~~~
(c -> 'f', a -> 1, b -> 2)
~~~

tampa, pvz.,

~~~
[ 'c', 'f', 'a', 1, 'b', 2 ]
~~~


## Žaidimo variacija (laivų figūros):
### Tetris
~~~
QQQQ

QQ
 QQ
 
QQ
QQ

 Q
QQQ

QQQ
  Q
~~~

## Validavimas

Ką reikia validuoti:
1. Protokolą (ar, pvz., json yra json)
2. Ar kiekvienas langelis turi max. vieną ėjimą.

### Padaryti ėjimą (korektišką bet nebūtinai prasmingą). Signatūra:

~~~haskell
move :: String -> Either String (Maybe [String])
~~~


## 1 Užduotis -
Move/Tetris, json be žodynų

## 2 Užduotis - 
Užduotis antsiskaitoma poromis. Pirmas (-a) studentas (-ė) "A" pradeda žaidimą (atakuoja), o kitas (-a) -- "B" -- ginasi.
Žaidimo partnerį galima pasirinkti laisvai, tik abudu partneriai privalo naudoti skirtingus žinučių formatus.

Žaidimas vyksta per HTTP protokolą. Naudojami 2 metodai

- POST -- padaro ėjimą (siunčia jį partneriui)
- GET -- gauna (laukia) partnerio ėjimą.

Parašyti robotą (komandinės eilutės programą) komunikuojantį per resursą [http://battleship.haskell.lt/](http://battleship.haskell.lt/).

0. Abudu partneriai privalo naudoti skirtingus žinučių formatus
1. Tvarkingas Cabal/Stack projektas
2. Kompiliavimas į binarinį vykdomą failą be Warningų
3. Atsiskaitymas poromis
4. Ėjimų optimizavimas (bent vienas) priklausomai nuo žaidimo varianto (bet atsitiktiniai ėjimai yra nedraudžiami)
5. Pirmos užduoties (parserio) (per-)panaudojimas
6. Programa turi palaikyti abudu žaidimo scenarijus: "A" ir "B"
7. Parametrus programa nuskaito iš komadinės eilutės argumentų, pvz., "battleship A zaidimo_id" - (nereikalautų perkompiliavimo)
