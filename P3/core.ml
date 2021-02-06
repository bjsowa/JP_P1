open Syntax
open Support.Pervasive
open Support.Error

(* ---------------------  CONTEXT MANAGEMENT  -------------------- *)

let emptycontext = []

let add_binding ctx x typ = (x, typ) :: ctx

let lookup fi ctx x =
  try List.assoc x ctx
  with Not_found ->
    let msg =
      Printf.sprintf
        "Variable type lookup failure: Variable %s not found in context"
    in
    error fi (msg x)

(* --------------------  EXTRACTING FILE INFO  ------------------- *)

let tmInfo t =
  match t with
  | TmVar (fi, _) -> fi
  | TmAbs (fi, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmUnit fi -> fi
  | _ -> dummyinfo

(* --------------------------  PRINTING  ------------------------- *)

let rec printty_Type typ =
  match typ with
  | TyFunc (typ1, typ2) ->
      printty_AType typ1;
      pr "->";
      printty_Type typ2
  | _ -> printty_AType typ

and printty_AType typ =
  match typ with
  | TyUnit -> pr "Unit"
  | _ ->
      pr "(";
      printty_Type typ;
      pr ")"

let printty typ = printty_Type typ

let rec printtm_Term t =
  match t with
  | TmAbs (_, x, t1) ->
      pr "lambda ";
      pr x;
      pr ".";
      printtm_Term t1
  | _ -> printtm_AppTerm t

and printtm_AppTerm t =
  match t with
  | TmApp (_, t1, t2) ->
      printtm_AppTerm t1;
      pr " ";
      printtm_ATerm t2
  | _ -> printtm_ATerm t

and printtm_ATerm t =
  match t with
  | TmVar (_, x) -> pr x
  | TmUnit _ -> pr "unit"
  | _ ->
      pr "(";
      printtm_Term t;
      pr ")"

let printtm t = printtm_Term t

