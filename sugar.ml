open Syntax

let ftrue fi = 
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 1)))

let ffalse fi = 
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 1)))

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