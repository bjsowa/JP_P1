# Rozwiązanie zadania nr 1 na pracownię z Języków Programowania.

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
`<plik>` musi zawierać polecenia dla normalizatora oddzielone średnikiem. \
Jeśli polecenie jest termem, program wypisze "odcukrzony", znormalizowany term. \
Jeśli poleceniem są dwa termy oddzielone słowem `==`, program sprawdzi beta-równość termów i wypisze `true` albo `false`. \
Jeśli flaga `-v` jest ustawiona, program wypisze na wyjściu dodatkowe informacje o ewaluowanych termach.

W folderze `tests.t` znajdują się przykładowe pliki wejściowe dla normalizatora.

[dune]: https://github.com/ocaml/dune