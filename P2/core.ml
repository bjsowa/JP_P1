open Syntax
open Support.Pervasive
open Support.Error

(* ---------------------  CONTEXT MANAGEMENT  -------------------- *)

let emptycontext = []

(* --------------------  EXTRACTING FILE INFO  ------------------- *)

let tmInfo t = match t with
    TmVar(fi,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi,_,_) -> fi 
  | TmNum(fi,_) -> fi
  | TmFix(fi,_) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmAdd(fi,_,_) -> fi
  | TmSub(fi,_,_) -> fi
  | TmMult(fi,_,_) -> fi
  | TmDiv(fi,_,_) -> fi
  | TmEq(fi,_,_) -> fi
  | TmAnd(fi,_,_) -> fi
  | TmOr(fi,_,_) -> fi
  | TmUnit(fi) -> fi

(* --------------------------  PRINTING  ------------------------- *)

let rec printty_Type typ = match typ with
  | TyFunc(typ1,typ2) -> 
      printty_AType typ1; pr "->"; printty_Type typ2
  | _ -> printty_AType typ

and printty_AType typ = match typ with
  | TyInt -> pr "Int"
  | TyBool -> pr "Bool"
  | TyUnit -> pr "Unit"
  | _ -> pr "("; printty_Type typ; pr ")"

let printty typ = printty_Type typ

let rec printtm_Term t = match t with
  | TmIf(_, t1, t2, t3) ->
      pr "if "; printtm_Term t1;
      pr "then "; printtm_Term t2;
      pr "else "; printtm_Term t3;
  | TmAbs(_,x,typ,t1) ->
      pr "lambda "; pr x; pr ":"; printty typ; pr "."; printtm_Term t1
  | TmAdd(_,t1,t2) ->
      printtm_ATerm t1; pr "+"; printtm_ATerm t2
  | TmSub(_,t1,t2) ->
      printtm_ATerm t1; pr "-"; printtm_ATerm t2
  | TmMult(_,t1,t2) ->
      printtm_ATerm t1; pr "*"; printtm_ATerm t2
  | TmDiv(_,t1,t2) ->
      printtm_ATerm t1; pr "/"; printtm_ATerm t2
  | TmEq(_,t1,t2) ->
      printtm_ATerm t1; pr "="; printtm_ATerm t2
  | TmAnd(_,t1,t2) ->
      printtm_ATerm t1; pr "&&"; printtm_ATerm t2
  | TmOr(_,t1,t2) ->
      printtm_ATerm t1; pr "||"; printtm_ATerm t2
  | _ -> printtm_AppTerm t

and printtm_AppTerm t = match t with
  | TmApp(_, t1, t2) ->
      printtm_AppTerm t1;
      printtm_ATerm t2
  | TmFix(_, t1) ->
      pr "fix "; printtm_ATerm t1
  | _ -> printtm_ATerm t

and printtm_ATerm t = match t with
  | TmVar(_,x) -> pr x
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmNum(_,x) -> Format.print_int x
  | TmUnit(_) -> pr "unit"
  | _ -> pr "("; printtm_Term t; pr ")"

let printtm t = printtm_Term t

(* ------------------------  TYPE CHECKING  ----------------------- *)

let lookup ctx x = 
  List.assoc x ctx

let rec infer_type ctx t = match t with
  | TmVar(_,x) -> lookup ctx x
  (* | TmAbs(_,x,t1) ->  *)
  | TmApp(fi,t1,t2) ->
      let typ = infer_type ctx t1 in (
        match typ with 
          | TyFunc(ty1, ty2) -> 
              if check_type ctx t2 ty1
              then ty2
              else error fi "Mismatched types: Wrong function argument"
          | _ -> error fi "Mismatched types: Not a function"
      )
  | TmNum(_,_) ->
      TyInt
  | TmFix(fi,t1) ->
      let typ = infer_type ctx t1 in (
        match typ with 
          | TyFunc(ty1, ty2) -> 
              if ty1 == ty2
              then ty1
              else error fi "Mismatched types: Not of shape (t -> t)" 
          | _ -> error fi "Mismatched types: Not a function"
      )
  | TmTrue(_)
  | TmFalse(_) -> TyBool
  | TmAdd(fi,t1,t2)
  | TmSub(fi,t1,t2)
  | TmMult(fi,t1,t2)
  | TmDiv(fi,t1,t2) ->
      if check_type ctx t1 TyInt && check_type ctx t2 TyInt
      then TyInt
      else error fi "Mismatched types: Not an integer"
  | TmEq(fi,t1,t2) ->
      if check_type ctx t1 TyInt && check_type ctx t2 TyInt
      then TyBool
      else error fi "Mismatched types: Not an integer" 
  | TmAnd(fi,t1,t2)
  | TmOr(fi,t1,t2) ->
      if check_type ctx t1 TyBool && check_type ctx t2 TyBool
      then TyBool
      else error fi "Mismatched types: Not a boolean" 
  | TmIf(fi,t1,t2,t3) ->
      if check_type ctx t1 TyBool
      then 
        let typ1 = infer_type ctx t2 in
        if check_type ctx t3 typ1
        then typ1
        else error fi "Mismatched types: If cases not match"
      else error fi "Mismatched types: If condition is not a boolean"
  | TmUnit(_) ->
      TyUnit
  | _ -> TyBool 

and check_type ctx t typ = match t with
  | TmApp(_,t1,t2) ->
      let typ1 = infer_type ctx t2 in
      check_type ctx t1 (TyFunc(typ1, typ))
  | TmFix(_,t1) ->
      check_type ctx t1 (TyFunc(typ, typ))
  | TmIf(_,t1,t2,t3) ->
      check_type ctx t1 TyBool &&
      check_type ctx t2 typ &&
      check_type ctx t3 typ
  | _ -> 
      let typ2 = infer_type ctx t in
      typ == typ2
