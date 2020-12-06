open Syntax

(* Booleans *)

let ftrue fi = 
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 1)))

let ffalse fi = 
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 1)))

(* Numerals *)

let num fi n =
  let rec church n = (
    if n == 0 then TVar(fi, 0)
    else TApp(fi, TVar(fi, 1), church (n-1))
  ) in
  TAbs(fi, "s", TAbs(fi, "z", church n))

let add fi = 
  TAbs(fi, "m", 
    TAbs(fi, "n", 
      TAbs(fi, "s", 
        TAbs(fi, "z", 
          TApp(fi,
            TApp(fi,
              TVar(fi, 3),
              TVar(fi, 1) ),
            TApp(fi,
              TApp(fi, 
                TVar(fi, 2),
                TVar(fi, 1)
              ),
              TVar(fi, 0) ) ) ) ) ) )

let mult fi =
  TAbs(fi, "m", 
    TAbs(fi, "n", 
      TAbs(fi, "s", 
        TAbs(fi, "z", 
          TApp(fi,
            TApp(fi,
              TVar(fi, 3),
              TApp(fi,
                TVar(fi, 2),
                TVar(fi, 1) ) ),
            TVar(fi, 0) ) ) ) ) )

(* Y-combinator *)

let fix fi = 
  TAbs(fi, "f",
    TApp(fi,
      TAbs(fi, "x",
        TApp(fi,
          TVar(fi, 1),
          TApp(fi,
            TVar(fi, 0),
            TVar(fi, 0) ) ) ),
      TAbs(fi, "x",
        TApp(fi,
          TVar(fi, 1),
          TApp(fi,
            TVar(fi, 0),
            TVar(fi, 0) ) ) ) ) )

(* Pairs *)

let pair fi =
  TAbs(fi, "f",
    TAbs(fi, "s",
      TAbs(fi, "b",
        TApp(fi,
          TApp(fi,
            TVar(fi, 0),
            TVar(fi, 2)),
          TVar(fi, 1) ) ) ) )

let fst fi =
  TAbs(fi, "p", TApp(fi, TVar(fi, 0), ftrue fi))

let snd fi =
  TAbs(fi, "p", TApp(fi, TVar(fi, 0), ffalse fi))

(* Lists *)

let nil fi =
  TApp(fi, TApp(fi, pair fi, ftrue fi), ftrue fi)