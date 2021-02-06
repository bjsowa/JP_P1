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

(* --------------------------  TYPE CHECKING  ------------------------- *)

let type_counter = ref 0

let fresh_type_variable () =
  let ret = !type_counter in
  type_counter := ret + 1;
  CtyVar ret

let rec infer_type ctx t =
  match t with
  | TmVar (fi, x) ->
      let typ = lookup fi ctx x in
      (typ, [])
  | TmAbs (_, x, t1) ->
      let fresh = fresh_type_variable () in
      let ctx1 = add_binding ctx x fresh in
      let typ, cstrs = infer_type ctx1 t1 in
      (CtyFunc (fresh, typ), cstrs)
  | TmApp (_, t1, t2) ->
      let typ1, cstrs1 = infer_type ctx t1 in
      let typ2, cstrs2 = infer_type ctx t2 in
      let fresh = fresh_type_variable () in
      let cstr = (CtyFunc (typ2, fresh), typ1) in
      (fresh, cstr :: List.append cstrs1 cstrs2)
  | TmUnit _ -> (CtyUnit, [])
  | TmProd (_, t1, t2) ->
      let typ1, cstrs1 = infer_type ctx t1 in
      let typ2, cstrs2 = infer_type ctx t2 in
      (CtyProd (typ1, typ2), List.append cstrs1 cstrs2)
  | TmProj (_, t1, id) ->
      let fresh1 = fresh_type_variable () in
      let fresh2 = fresh_type_variable () in
      let typ1, cstrs1 = infer_type ctx t1 in
      let cstr = (typ1, CtyProd (fresh1, fresh2)) in
      ((match id with ID_1 -> fresh1 | ID_2 -> fresh2), cstr :: cstrs1)
  | TmAbort (_, t1) ->
      let typ1, cstrs1 = infer_type ctx t1 in
      let fresh = fresh_type_variable () in
      let cstr = (typ1, CtyVoid) in
      (fresh, cstr :: cstrs1)
  | TmIn (_, id, t1) ->
      let typ1, cstrs1 = infer_type ctx t1 in
      let fresh = fresh_type_variable () in
      let ctyp =
        match id with
        | ID_1 -> CtySum (typ1, fresh)
        | ID_2 -> CtySum (fresh, typ1)
      in
      (ctyp, cstrs1)
  | TmCase (_, t1, (x2, t2), (x3, t3)) ->
      let typ1, cstrs1 = infer_type ctx t1 in
      let fresh1 = fresh_type_variable () in
      let fresh2 = fresh_type_variable () in
      let ctx2 = add_binding ctx x2 fresh1 in
      let typ2, cstrs2 = infer_type ctx2 t2 in
      let ctx3 = add_binding ctx x3 fresh2 in
      let typ3, cstrs3 = infer_type ctx3 t3 in
      let cstr1 = typ1, CtySum (fresh1, fresh2) in
      let cstr2 = typ2, typ3 in
      typ2, (List.concat [[cstr1;cstr2];cstrs1; cstrs2; cstrs3])
  (* | _ -> (CtyUnit, []) *)
