# Rozwiązanie zadania nr 2 na pracownię z Języków Programowania.

## Wymagania
- [dune]

## Budowanie
Aby zbudować projekt, wystarczy wywołać:
```
dune build
```
Aby uruchomić testy cramowe:
```
dune test
```

## Używanie
```
dune exec -- ./main.exe [-v] <plik>
```

`<plik>` musi zawierać polecenia dla ewaluatora oddzielone średnikiem.
Dla polecenia postaci `<term>` program wypisze wartość wyrażenia po ewaluacji.
Jeśli polecenie jest postaci `typeof <term>` program wypisze wyinferowany typ wyrażenia.

Jeśli flaga `-v` jest ustawiona, program wypisze na wyjściu dodatkowe informacje o ewaluowanych termach.

W folderze `tests.t` znajdują się przykładowe pliki wejściowe dla ewaluatora.

## Zakres realizacji zadania

Program implementuję inferencję typów i ewaluator call-by-value dla rachunku lambda z typami prostymi à la Church rozbudowanego o:
 - liczby naturalne 
 - wartości boolowskie
 - operacje arytmetyczne (dodawanie, odejmowanie, mnożenie, dzielenie)
 - operacje logiczne (koniunkcja i alternatywa)
 - konstrucję fix
 - wyjątki
 - typ `Unit`

Ewaluator jest zaimplementowany w stylu maszyny abstrakcyjnej CEK (Control-Environment-Kontinuation).

[dune]: https://github.com/ocaml/dune