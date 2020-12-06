open Syntax


(* Booleans *)

let ftrue fi = (* λt. λf. t *)
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 1)))

let ffalse fi = (* λt. λf. f *)
  TAbs(fi, "t", TAbs(fi, "f", TVar(fi, 0)))


(* Numerals *)

let num fi n = (* λs. λz. s (s (s ... ( s z ) ... ) *)
  let rec church n = (
    if n == 0 then TVar(fi, 0)
    else TApp(fi, TVar(fi, 1), church (n-1))
  ) in
  TAbs(fi, "s", TAbs(fi, "z", church n))

let add fi = (* λm. λn. λs. λz. m s (n s z) *)
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

let mult fi = (* λm. λn. λs. λz. m (n s) z *)
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

let succ fi = (* λn. λs. λz. s (n s z) *)
  TAbs(fi, "n",
    TAbs(fi, "s",
      TAbs(fi, "z",
        TApp(fi,
          TVar(fi, 1),
          TApp(fi,
            TApp(fi,
              TVar(fi, 2),
              TVar(fi, 1) ),
            TVar(fi, 0) ) ) ) ) )

let pred fi = (* λn. λs. λz. n (λg. λh. h (g s)) (λu. z) (λu. u) *)
  TAbs(fi, "n",
    TAbs(fi, "s",
      TAbs(fi, "z",
        TApp(fi,
          TApp(fi,
            TApp(fi,
              TVar(fi, 2),
              TAbs(fi, "g",
                TAbs(fi, "h",
                  TApp(fi,
                    TVar(fi, 0),
                    TApp(fi,
                      TVar(fi, 1),
                      TVar(fi, 3) ) ) ) ) ),
            TAbs(fi, "u",
              TVar(fi, 1) ) ),
          TAbs(fi, "u",
            TVar(fi, 0) ) ) ) ) )
   
let sub fi = (* λm. λn. n pred m *)
  TAbs(fi, "m",
    TAbs(fi, "n",
      TApp(fi,
        TApp(fi,
          TVar(fi, 0),
          pred fi),
        TVar(fi, 1) ) ) )

let iszero fi = (* λn. n (λx. false) true *)
  TAbs(fi, "n",
    TApp(fi,
      TApp(fi,
        TVar(fi, 0),
        TAbs(fi, "x",
          ffalse fi) ),
      ftrue fi) )

let leq fi = (* λm. λn. iszero (sub m n) *)
  TAbs(fi, "m",
    TAbs(fi, "n",
      TApp(fi,
        iszero fi,
        TApp(fi,
          TApp(fi,
            sub fi,
            TVar(fi, 1) ),
          TVar(fi, 0) ) ) ) )

let eq fi = (* λm. λn. (leq m n) (leq n m) false *)
  TAbs(fi, "m",
    TAbs(fi, "n",
      TApp(fi,
        TApp(fi,
          TApp(fi,
            TApp(fi,
              leq fi,
              TVar(fi, 1) ),
            TVar(fi, 0) ),
          TApp(fi,
            TApp(fi,
              leq fi,
              TVar(fi, 0) ),
            TVar(fi, 1) ) ),
        ffalse fi ) ) )
    

(* Fixed-point combinator *)

let fix fi = (* λf. (λx. f (x x)) (λx. f (x x)) *)
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

let pair fi = (* λf. λs. λb. b f s *)
  TAbs(fi, "f",
    TAbs(fi, "s",
      TAbs(fi, "b",
        TApp(fi,
          TApp(fi,
            TVar(fi, 0),
            TVar(fi, 2)),
          TVar(fi, 1) ) ) ) )

let fst fi = (* λp. p true *)
  TAbs(fi, "p", TApp(fi, TVar(fi, 0), ftrue fi))

let snd fi = (* λp. p false *)
  TAbs(fi, "p", TApp(fi, TVar(fi, 0), ffalse fi))


(* Lists *)

let nil fi = (* pair true true *)
  TApp(fi, TApp(fi, pair fi, ftrue fi), ftrue fi)

let isnil fi = (* fst *)
  fst fi

let cons fi = (* λh. λt. pair false (pair h t) *)
  TAbs(fi, "h",
    TAbs(fi, "t",
      TApp(fi,
        TApp(fi,
          pair fi,
          ffalse fi),
        TApp(fi,
          TApp(fi,
            pair fi,
            TVar(fi, 1) ),
          TVar(fi, 0) ) ) ) )

let head fi = (* λz. fst (snd z) *)
  TAbs(fi, "z",
    TApp(fi, 
      fst fi,
      TApp(fi,
        snd fi,
        TVar(fi, 0) ) ) )

let tail fi = (* λz. snd (snd z) *)
  TAbs(fi, "z",
    TApp(fi, 
      snd fi,
      TApp(fi,
        snd fi,
        TVar(fi, 0) ) ) )