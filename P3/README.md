# Rozwiązanie zadania nr 3 na pracownię z Języków Programowania.

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

`<plik>` musi zawierać termy zakończone znakiem średnika.

Jeśli flaga `-v` jest ustawiona, program wypisze na wyjściu dodatkowe informacje o ewaluowanych termach.

W folderze `tests.t` znajdują się przykładowe pliki wejściowe dla programu.

## Zakres realizacji zadania

Program implementuje inferencję typów dla języka z typami prostymi (bez let-polimorfizmu). Konstrukcja let nie została dodana, ponieważ zabrakło mi czasu. 

Krótkie wyjaśnienie niektórych typów danych zdefiniowanych w `syntax.mli`:
  - `ty` - typy zamknięte
  - `cty` - typy ze zmiennymi unifikacyjnymi
  - `tyann` - anotacja typowa dla wyrażeń.
  - `term` - składnia opisująca zarówno wyrażenia nieotypowane jak i otypowane. Informacje o typie trzymane są w anotacjach typowych.

Działanie:
  - `infer_type` - Dostaje kontekst oraz nieotypowany term. Zwraca term ze zmienionymi anotacjami typowymi na typy ze zmiennymi unifikacyjnymi oraz listę wiązań typowych.
  - `unify` - Dostaje term anotowany typami ze zmiennymi unifikacyjnymi oraz listę wiązań typowych. Wykorzystuje algorytm Huet do rozwiązania problemu unifikacji. Następnie zwraca term, w którym anotacje typowe wskazują już na typy zamknięte oraz typ całego termu. Jeśli jakaś zmienna unifikacyjna nie zunifikowała się z żadnym typem zamkniętym, zostanie ona zunifikowana z typem Unit. Dodatkowo, operacja `find` wykrywa cykle, dzięki czemu funkcja nie pętli się na termach, które są `infinitely unifiable`.

[dune]: https://github.com/ocaml/dune