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

(* -----------------------  EXTRACTING INFO  ---------------------- *)

let tmInfo t =
  match t with
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _, _) -> fi
  | TmUnit (fi, _) -> fi
  | TmProd (fi, _, _, _) -> fi
  | TmProj (fi, _, _, _) -> fi
  | TmAbort (fi, _, _) -> fi
  | TmIn (fi, _, _, _) -> fi
  | TmCase (fi, _, _, _, _) -> fi

let tmTyann t =
  match t with
  | TmVar (_, ann, _) -> ann
  | TmAbs (_, ann, _, _) -> ann
  | TmApp (_, ann, _, _) -> ann
  | TmUnit (_, ann) -> ann
  | TmProd (_, ann, _, _) -> ann
  | TmProj (_, ann, _, _) -> ann
  | TmAbort (_, ann, _) -> ann
  | TmIn (_, ann, _, _) -> ann
  | TmCase (_, ann, _, _, _) -> ann

(* --------------------------  PRINTING  ------------------------- *)

let rec printty_Type typ =
  match typ with
  | TyFunc (typ1, typ2) ->
      printty_AType typ1;
      pr " -> ";
      printty_Type typ2
  | TyProd (typ1, typ2) ->
      printty_AType typ1;
      pr " x ";
      printty_AType typ2
  | TySum (typ1, typ2) ->
      printty_AType typ1;
      pr " + ";
      printty_AType typ2
  | _ -> printty_AType typ

and printty_AType typ =
  match typ with
  | TyUnit -> pr "Unit"
  | TyVoid -> pr "Void"
  | _ ->
      pr "(";
      printty_Type typ;
      pr ")"

let printty typ = printty_Type typ

let rec printcty_Type typ =
  match typ with
  | CtyFunc (typ1, typ2) ->
      printcty_AType typ1;
      pr " -> ";
      printcty_Type typ2
  | CtyProd (typ1, typ2) ->
      printcty_AType typ1;
      pr " x ";
      printcty_AType typ2
  | CtySum (typ1, typ2) ->
      printcty_AType typ1;
      pr " + ";
      printcty_AType typ2
  | _ -> printcty_AType typ

and printcty_AType typ =
  match typ with
  | CtyVar v ->
      pr "v";
      Format.print_int v
  | CtyUnit -> pr "Unit"
  | CtyVoid -> pr "Void"
  | _ ->
      pr "(";
      printcty_Type typ;
      pr ")"

let printcty typ = printcty_Type typ

let printconstr (typ1, typ2) =
  printcty typ1;
  pr " = ";
  printcty typ2

let rec printtm_Term t =
  match t with
  | TmAbs (_, _, x, t1) ->
      pr "lambda ";
      pr x;
      pr ".";
      printtm_Term t1
  | _ -> printtm_AppTerm t

and printtm_AppTerm t =
  match t with
  | TmApp (_, _, t1, t2) ->
      printtm_AppTerm t1;
      pr " ";
      printtm_ATerm t2
  | _ -> printtm_ATerm t

and printtm_ATerm t =
  match t with
  | TmVar (_, _, x) -> pr x
  | TmUnit (_, _) -> pr "unit"
  | TmProd (_, _, t1, t2) ->
      pr "<";
      printtm_Term t1;
      pr ",";
      printtm_Term t2;
      pr ">"
  | TmProj (_, _, t1, id) ->
      pr "p";
      printid id;
      pr " ";
      printtm_ATerm t1
  | TmAbort (_, _, t1) ->
      pr "abort ";
      printtm_ATerm t1
  | TmIn (_, _, id, t1) ->
      pr "in";
      printid id;
      pr " ";
      printtm_ATerm t1
  | TmCase (_, _, t1, (x2, t2), (x3, t3)) ->
      pr "case ";
      printtm_ATerm t1;
      pr " of in1 ";
      pr x2;
      pr " => ";
      printtm_ATerm t2;
      pr " | in2 ";
      pr x3;
      pr " => ";
      printtm_ATerm t3
  | _ ->
      pr "(";
      printtm_Term t;
      pr ")"

and printid id = Format.print_int (match id with ID_1 -> 1 | _ -> 2)

let printtm t = printtm_Term t

let printtyann ann =
  match ann with
  | Unknown -> pr "Unknown"
  | Type typ ->
      pr "Type ";
      printty typ
  | CType typ ->
      pr "CType ";
      printcty typ

let rec printtm_ann t =
  ( match t with
  | TmAbs (_, _, _, t1) -> printtm_ann t1
  | TmApp (_, _, t1, t2) ->
      printtm_ann t1;
      printtm_ann t2
  | TmProd (_, _, t1, t2) ->
      printtm_ann t1;
      printtm_ann t2
  | TmProj (_, _, t1, _) -> printtm_ann t1
  | TmAbort (_, _, t1) -> printtm_ann t1
  | TmIn (_, _, _, t1) -> printtm_ann t1
  | TmCase (_, _, t1, (_, t2), (_, t3)) ->
      printtm_ann t1;
      printtm_ann t2;
      printtm_ann t3
  | _ -> () );
  printtm t;
  pr " : ";
  printtyann (tmTyann t);
  pr "\n"

let printuarr uarr =
  Array.iteri
    (fun i ctyp ->
      pr "v";
      print_int i;
      pr " => ";
      printcty ctyp;
      pr "\n")
    uarr

(* --------------------------  TYPE CHECKING  ------------------------- *)

let type_counter = ref 0

let reset_type_counter () = type_counter := 0

let fresh_type_variable () =
  let ret = !type_counter in
  type_counter := ret + 1;
  CtyVar ret

let rec infer_type ctx t =
  match t with
  | TmVar (fi, _, x) ->
      let typ = lookup fi ctx x in
      (TmVar (fi, CType typ, x), typ, [])
  | TmAbs (fi, _, x, t1) ->
      let fresh = fresh_type_variable () in
      let ctx1 = add_binding ctx x fresh in
      let tann1, typ1, cstrs1 = infer_type ctx1 t1 in
      let typ = CtyFunc (fresh, typ1) in
      (TmAbs (fi, CType typ, x, tann1), typ, cstrs1)
  | TmApp (fi, _, t1, t2) ->
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let tann2, typ2, cstrs2 = infer_type ctx t2 in
      let typ = fresh_type_variable () in
      let cstr = (CtyFunc (typ2, typ), typ1) in
      ( TmApp (fi, CType typ, tann1, tann2),
        typ,
        cstr :: List.append cstrs1 cstrs2 )
  | TmUnit (fi, _) -> (TmUnit (fi, CType CtyUnit), CtyUnit, [])
  | TmProd (fi, _, t1, t2) ->
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let tann2, typ2, cstrs2 = infer_type ctx t2 in
      let typ = CtyProd (typ1, typ2) in
      (TmProd (fi, CType typ, tann1, tann2), typ, List.append cstrs1 cstrs2)
  | TmProj (fi, _, t1, id) ->
      let fresh1 = fresh_type_variable () in
      let fresh2 = fresh_type_variable () in
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let typ = match id with ID_1 -> fresh1 | ID_2 -> fresh2 in
      let cstr = (typ1, CtyProd (fresh1, fresh2)) in
      (TmProj (fi, CType typ, tann1, id), typ, cstr :: cstrs1)
  | TmAbort (fi, _, t1) ->
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let typ = fresh_type_variable () in
      let cstr = (typ1, CtyVoid) in
      (TmAbort (fi, CType typ, tann1), typ, cstr :: cstrs1)
  | TmIn (fi, _, id, t1) ->
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let fresh = fresh_type_variable () in
      let typ =
        match id with
        | ID_1 -> CtySum (typ1, fresh)
        | ID_2 -> CtySum (fresh, typ1)
      in
      (TmIn (fi, CType typ, id, tann1), typ, cstrs1)
  | TmCase (fi, _, t1, (x2, t2), (x3, t3)) ->
      let tann1, typ1, cstrs1 = infer_type ctx t1 in
      let fresh1 = fresh_type_variable () in
      let fresh2 = fresh_type_variable () in
      let ctx2 = add_binding ctx x2 fresh1 in
      let tann2, typ2, cstrs2 = infer_type ctx2 t2 in
      let ctx3 = add_binding ctx x3 fresh2 in
      let tann3, typ3, cstrs3 = infer_type ctx3 t3 in
      let cstr1 = (typ1, CtySum (fresh1, fresh2)) in
      let cstr2 = (typ2, typ3) in
      ( TmCase (fi, CType typ2, tann1, (x2, tann2), (x3, tann3)),
        typ2,
        List.concat [ [ cstr1; cstr2 ]; cstrs1; cstrs2; cstrs3 ] )

let init_unification_array () =
  let uarr : unification_array = Array.init !type_counter (fun i -> CtyVar i) in
  uarr

let rec find ctyp uarr visited =
  match ctyp with
  | CtyVar i ->
      if List.mem i visited then
        err "Unification failed: infinitely unifiable term"
      else
        let utyp = uarr.(i) in
        if utyp = ctyp then utyp
        else
          let utyp1 = find utyp uarr (i :: visited) in
          uarr.(i) <- utyp1;
          utyp1
  | CtyUnit -> CtyUnit
  | CtyVoid -> CtyVoid
  | CtyFunc (ctyp1, ctyp2) ->
      let utyp1 = find ctyp1 uarr visited in
      let utyp2 = find ctyp2 uarr visited in
      CtyFunc (utyp1, utyp2)
  | CtyProd (ctyp1, ctyp2) ->
      let utyp1 = find ctyp1 uarr visited in
      let utyp2 = find ctyp2 uarr visited in
      CtyProd (utyp1, utyp2)
  | CtySum (ctyp1, ctyp2) ->
      let utyp1 = find ctyp1 uarr visited in
      let utyp2 = find ctyp2 uarr visited in
      CtySum (utyp1, utyp2)

let rec process_constraints cstrs uarr =
  match cstrs with
  | [] -> ()
  | ((ctyp1, ctyp2) as cstr) :: cstrs1 -> (
      let u = find ctyp1 uarr [] in
      let v = find ctyp2 uarr [] in
      match (u, v) with
      | CtyFunc (cc11, cc12), CtyFunc (cc21, cc22)
      | CtyProd (cc11, cc12), CtyProd (cc21, cc22)
      | CtySum (cc11, cc12), CtySum (cc21, cc22) ->
          process_constraints ((cc11, cc21) :: (cc12, cc22) :: cstrs1) uarr
      | CtyUnit, CtyUnit | CtyVoid, CtyVoid -> process_constraints cstrs1 uarr
      | CtyVar i1, CtyVar i2 ->
          uarr.(i1) <- CtyVar i2;
          process_constraints cstrs1 uarr
      | CtyVar i1, c1 | c1, CtyVar i1 ->
          uarr.(i1) <- c1;
          process_constraints cstrs1 uarr
      | _ ->
          errf (fun () ->
              pr "Error: Unification failed: Can't apply constraint: ";
              printconstr cstr) )

let rec ctype_to_type ctyp =
  match ctyp with
  | CtyVar _ | CtyUnit -> TyUnit
  | CtyVoid -> TyVoid
  | CtyFunc (ctyp1, ctyp2) ->
      let typ1 = ctype_to_type ctyp1 in
      let typ2 = ctype_to_type ctyp2 in
      TyFunc (typ1, typ2)
  | CtyProd (ctyp1, ctyp2) ->
      let typ1 = ctype_to_type ctyp1 in
      let typ2 = ctype_to_type ctyp2 in
      TyProd (typ1, typ2)
  | CtySum (ctyp1, ctyp2) ->
      let typ1 = ctype_to_type ctyp1 in
      let typ2 = ctype_to_type ctyp2 in
      TySum (typ1, typ2)

let subst_tyann ann uarr =
  match ann with
  | CType ctyp -> Type (ctype_to_type (find ctyp uarr []))
  | _ -> err "Trying to substitute a non-constrained type"

let rec subst_term t uarr =
  match t with
  | TmVar (fi, ann, t1) ->
      let ann1 = subst_tyann ann uarr in
      TmVar (fi, ann1, t1)
  | TmAbs (fi, ann, x, t1) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      TmAbs (fi, ann1, x, st1)
  | TmApp (fi, ann, t1, t2) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      let st2 = subst_term t2 uarr in
      TmApp (fi, ann1, st1, st2)
  | TmProd (fi, ann, t1, t2) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      let st2 = subst_term t2 uarr in
      TmProd (fi, ann1, st1, st2)
  | TmProj (fi, ann, t1, id) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      TmProj (fi, ann1, st1, id)
  | TmAbort (fi, ann, t1) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      TmAbort (fi, ann1, st1)
  | TmIn (fi, ann, id, t1) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      TmIn (fi, ann1, id, st1)
  | TmCase (fi, ann, t1, (x2, t2), (x3, t3)) ->
      let ann1 = subst_tyann ann uarr in
      let st1 = subst_term t1 uarr in
      let st2 = subst_term t2 uarr in
      let st3 = subst_term t3 uarr in
      TmCase (fi, ann1, st1, (x2, st2), (x3, st3))
  | _ -> t

let get_type ann =
  match ann with
  | Type typ -> typ
  | _ -> err "Internal Error: Expected type annotation"

let unify t cstrs =
  let uarr = init_unification_array () in
  process_constraints cstrs uarr;
  let t1 = subst_term t uarr in
  let typ = get_type (tmTyann t1) in
  (t1, typ)
