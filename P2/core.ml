open Syntax
open Support.Pervasive
open Support.Error

(* ---------------------  CONTEXT MANAGEMENT  -------------------- *)

let emptycontext = { variables = []; exceptions = [] }

let add_binding bindings x typ = (x, typ) :: bindings

let add_variable_binding ctx x typ =
  { ctx with variables = add_binding ctx.variables x typ }

let add_exception_binding ctx x typ =
  { ctx with exceptions = add_binding ctx.exceptions x typ }

let lookup bindings x = List.assoc x bindings

let lookup_variable fi ctx x =
  try lookup ctx.variables x
  with Not_found ->
    let msg =
      Printf.sprintf "Variable lookup failure: Variable %s not found in context"
    in
    error fi (msg x)

let lookup_exception fi ctx x =
  try lookup ctx.exceptions x
  with Not_found ->
    let msg =
      Printf.sprintf
        "Exception lookup failure: Exception %s not found in context"
    in
    error fi (msg x)

(* --------------------  EXTRACTING FILE INFO  ------------------- *)

let tmInfo t =
  match t with
  | TmVar (fi, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmNum (fi, _) -> fi
  | TmFix (fi, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmAdd (fi, _, _) -> fi
  | TmSub (fi, _, _) -> fi
  | TmMult (fi, _, _) -> fi
  | TmDiv (fi, _, _) -> fi
  | TmEq (fi, _, _) -> fi
  | TmAnd (fi, _, _) -> fi
  | TmOr (fi, _, _) -> fi
  | TmUnit fi -> fi
  | TmException (fi, _, _, _) -> fi
  | TmThrow (fi, _, _, _) -> fi
  | TmTry (fi, _, _) -> fi

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
  | TyInt -> pr "Int"
  | TyBool -> pr "Bool"
  | TyUnit -> pr "Unit"
  | _ ->
      pr "(";
      printty_Type typ;
      pr ")"

let printty typ = printty_Type typ

let rec printtm_Term t =
  match t with
  | TmIf (_, t1, t2, t3) ->
      pr "if ";
      printtm_Term t1;
      pr " then ";
      printtm_Term t2;
      pr " else ";
      printtm_Term t3
  | TmAbs (_, x, typ, t1) ->
      pr "lambda ";
      pr x;
      pr ":";
      printty typ;
      pr ".";
      printtm_Term t1
  | TmLet (_, x, t1, t2) ->
      pr "let ";
      pr x;
      pr "=";
      printtm_Term t1;
      pr " in ";
      printtm_Term t2
  | TmAdd (_, t1, t2) ->
      printtm_ATerm t1;
      pr "+";
      printtm_ATerm t2
  | TmSub (_, t1, t2) ->
      printtm_ATerm t1;
      pr "-";
      printtm_ATerm t2
  | TmMult (_, t1, t2) ->
      printtm_ATerm t1;
      pr "*";
      printtm_ATerm t2
  | TmDiv (_, t1, t2) ->
      printtm_ATerm t1;
      pr "/";
      printtm_ATerm t2
  | TmEq (_, t1, t2) ->
      printtm_ATerm t1;
      pr "=";
      printtm_ATerm t2
  | TmAnd (_, t1, t2) ->
      printtm_ATerm t1;
      pr "&&";
      printtm_ATerm t2
  | TmOr (_, t1, t2) ->
      printtm_ATerm t1;
      pr "||";
      printtm_ATerm t2
  | TmException (_, x, typ, t1) ->
      pr "exception ";
      pr x;
      pr " of ";
      printty typ;
      pr " in ";
      printtm_Term t1
  | TmThrow (_, x, t1, typ) ->
      pr "throw ";
      pr x;
      pr " ";
      printtm_Term t1;
      pr " as ";
      printty typ
  | TmTry (_, t1, c) ->
      pr "try ";
      printtm_Term t1;
      pr " catch ";
      List.iter
        (function
          | _, x1, x2, t2 ->
              pr "{";
              pr x1;
              pr " ";
              pr x2;
              pr " => ";
              printtm_Term t2;
              pr "}")
        c
  | _ -> printtm_AppTerm t

and printtm_AppTerm t =
  match t with
  | TmApp (_, t1, t2) ->
      printtm_AppTerm t1;
      pr " ";
      printtm_ATerm t2
  | TmFix (_, t1) ->
      pr "fix ";
      printtm_ATerm t1
  | _ -> printtm_ATerm t

and printtm_ATerm t =
  match t with
  | TmVar (_, x) -> pr x
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmNum (_, x) -> Format.print_int x
  | TmUnit _ -> pr "unit"
  | _ ->
      pr "(";
      printtm_Term t;
      pr ")"

let printtm t = printtm_Term t

let printv v =
  match v with
  | VInt v1 -> Format.print_int v1
  | VBool v1 -> Format.print_bool v1
  | _ -> pr "dupa"

(* ------------------------  TYPE CHECKING  ----------------------- *)

let rec infer_type ctx t =
  match t with
  | TmVar (fi, x) -> lookup_variable fi ctx x
  | TmAbs (_, x, typ, t1) ->
      let ctx1 = add_variable_binding ctx x typ in
      let typ1 = infer_type ctx1 t1 in
      TyFunc (typ, typ1)
  | TmLet (_, x, t1, t2) ->
      let typ = infer_type ctx t1 in
      let ctx1 = add_variable_binding ctx x typ in
      infer_type ctx1 t2
  | TmApp (fi, t1, t2) -> (
      let typ = infer_type ctx t1 in
      match typ with
      | TyFunc (ty1, ty2) ->
          if check_type ctx t2 ty1 then ty2
          else error fi "Mismatched types: Wrong function argument"
      | _ -> error fi "Mismatched types: Not a function" )
  | TmNum (_, _) -> TyInt
  | TmFix (fi, t1) -> (
      let typ = infer_type ctx t1 in
      match typ with
      | TyFunc (ty1, ty2) ->
          if ty1 == ty2 then ty1
          else error fi "Mismatched types: Not of shape (t -> t)"
      | _ -> error fi "Mismatched types: Not a function" )
  | TmTrue _ | TmFalse _ -> TyBool
  | TmAdd (fi, t1, t2)
  | TmSub (fi, t1, t2)
  | TmMult (fi, t1, t2)
  | TmDiv (fi, t1, t2) ->
      if check_type ctx t1 TyInt && check_type ctx t2 TyInt then TyInt
      else error fi "Mismatched types: Not an integer"
  | TmEq (fi, t1, t2) ->
      if check_type ctx t1 TyInt && check_type ctx t2 TyInt then TyBool
      else error fi "Mismatched types: Not an integer"
  | TmAnd (fi, t1, t2) | TmOr (fi, t1, t2) ->
      if check_type ctx t1 TyBool && check_type ctx t2 TyBool then TyBool
      else error fi "Mismatched types: Not a boolean"
  | TmIf (fi, t1, t2, t3) ->
      if check_type ctx t1 TyBool then
        let typ1 = infer_type ctx t2 in
        if check_type ctx t3 typ1 then typ1
        else error fi "Mismatched types: If then/else cases does not match"
      else error fi "Mismatched types: If condition is not a boolean"
  | TmUnit _ -> TyUnit
  | TmException (_, x, typ, t1) ->
      let ctx1 = add_exception_binding ctx x typ in
      infer_type ctx1 t1
  | TmThrow (fi, x, t1, typ) ->
      let typ1 = lookup_exception fi ctx x in
      if check_type ctx t1 typ1 then typ
      else error fi "Mismatched types: Wrong exception type"
  | TmTry (_, t1, c) ->
      let typ = infer_type ctx t1 in
      List.iter
        (function
          | fi1, x1, x2, t2 ->
              let typ1 = lookup_exception fi1 ctx x1 in
              let ctx1 = add_variable_binding ctx x2 typ1 in
              if not (check_type ctx1 t2 typ) then
                error fi1
                  "Mismatched types: Catch clause type does not match the try \
                   block type"
              else ())
        c;
      typ

and check_type ctx t typ =
  match t with
  | TmAbs (_, x, typ1, t1) -> (
      match typ with
      | TyFunc (ftyp1, ftyp2) ->
          typ1 == ftyp1
          &&
          let ctx1 = add_variable_binding ctx x typ1 in
          check_type ctx1 t1 ftyp2
      | _ -> false )
  | TmLet (_, x, t1, t2) ->
      let typ1 = infer_type ctx t1 in
      let ctx1 = add_variable_binding ctx x typ1 in
      check_type ctx1 t2 typ
  | TmException (_, x, typ, t2) ->
      let ctx1 = add_exception_binding ctx x typ in
      check_type ctx1 t2 typ
  | TmApp (_, t1, t2) ->
      let typ1 = infer_type ctx t2 in
      check_type ctx t1 (TyFunc (typ1, typ))
  | TmFix (_, t1) -> check_type ctx t1 (TyFunc (typ, typ))
  | TmIf (_, t1, t2, t3) ->
      check_type ctx t1 TyBool && check_type ctx t2 typ && check_type ctx t3 typ
  | _ ->
      let typ2 = infer_type ctx t in
      typ == typ2

(* -----------------------  EVALUATION  --------------------- *)

let vint_unpack v =
  match v with VInt v1 -> v1 | _ -> err "Cannot unpack int. Expected VInt."

let vbool_unpack v =
  match v with VBool v1 -> v1 | _ -> err "Cannot unpack bool. Expected VBool."

let rec eval_control t ctx =
  match t with
  | TmNum (_, x) -> eval_kontinuation (VInt x) ctx
  | TmTrue _ -> eval_kontinuation (VBool true) ctx
  | TmFalse _ -> eval_kontinuation (VBool false) ctx
  | TmIf (_, t1, t2, t3) -> eval_control t1 (CIf (t2, t3) :: ctx)
  | TmAdd (_, t1, t2) -> eval_control t1 (LAdd t2 :: ctx)
  | TmEq (_, t1, t2) -> eval_control t1 (LEq t2 :: ctx)
  | _ -> VUnit

and eval_kontinuation v ctx =
  match ctx with
  | [] -> v
  | CIf (t1, t2) :: ctx1 ->
      if vbool_unpack v then eval_control t1 ctx1 else eval_control t2 ctx1
  | LAdd t :: ctx1 -> eval_control t (RAdd v :: ctx1)
  | RAdd v1 :: ctx1 ->
      eval_kontinuation (VInt (vint_unpack v + vint_unpack v1)) ctx1
  | LEq t :: ctx1 -> eval_control t (REq v :: ctx1)
  | REq v1 :: ctx1 ->
      eval_kontinuation (VBool (vint_unpack v == vint_unpack v1)) ctx1
  | _ -> VUnit

let eval t = eval_control t []
