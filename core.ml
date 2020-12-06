open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ---------------------  CONTEXT MANAGEMENT  -------------------- *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addname ctx x = x::ctx

let index2name fi ctx x =
  try
    List.nth ctx x
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | y::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | y::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else (addname ctx x), x

(* ------------------  SHIFTING AND SUBSTITUTION ----------------- *)

let tmmap onvar c t = 
  let rec walk c t = match t with
    TVar(fi,x) -> onvar fi c x
  | TAbs(fi,x,t2) -> TAbs(fi,x,walk (c+1) t2)
  | TApp(fi,t1,t2) -> TApp(fi,walk c t1,walk c t2)
  in walk c t

let termShiftAbove d c t =
  tmmap
    (fun fi c x -> if x>=c then TVar(fi,x+d) else TVar(fi,x))
    c t

let termShift d t = termShiftAbove d 0 t

let termSubst j s t =
  tmmap
    (fun fi c x -> if x=j+c then termShift c s else TVar(fi,x))
    0
    t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)
 
(* --------------------  EXTRACTING FILE INFO  ------------------- *)

let tmInfo t = match t with
    TVar(fi,_) -> fi
  | TAbs(fi,_,_) -> fi
  | TApp(fi, _, _) -> fi 

let tmsInfo t = match t with
    TcsVar(fi,_) -> fi
  | TcsAbs(fi,_,_) -> fi
  | TcsApp(fi, _, _) -> fi 
  | TcsNum(fi,_) -> fi
  | TcsTrue(fi) -> fi
  | TcsFalse(fi) -> fi
  | TcsIf(fi,_,_,_) -> fi
  | TcsAdd(fi,_,_) -> fi
  | TcsMult(fi,_,_) -> fi
  | TcsFix(fi,_) -> fi
  | TcsPair(fi,_,_) -> fi
  | TcsFst(fi,_) -> fi
  | TcsSnd(fi,_) -> fi
  | TcsNil(fi) -> fi

(* --------------------------  PRINTING  ------------------------- *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TVar(_,_) -> true
  | _ -> false

let rec printtm_Term outer ctx t = match t with
    TAbs(_,x,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
            obox(); pr "lambda "; pr x'; pr ".";
            if (small t2) && not outer then break() else print_space();
            printtm_Term outer ctx' t2;
            cbox())
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TApp(_, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TVar(fi,x) -> pr (index2name fi ctx x)
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

(* ----------------------  CONVERTING TERMS  --------------------- *)

let rec bind_free_variables1 ctx_free ctx_bound t = match t with
  | TcsVar(_,x) ->
      if not (isnamebound ctx_bound x)
      then addname ctx_free x
      else ctx_free
  | TcsAbs(_,x,t1) ->
      let ctx_bound1 = addname ctx_bound x in
      bind_free_variables1 ctx_free ctx_bound1 t1
  | TcsApp(_,t1,t2) 
  | TcsAdd(_,t1,t2)
  | TcsMult(_,t1,t2)
  | TcsPair(_,t1,t2) -> 
      let ctx_free1 = bind_free_variables1 ctx_free ctx_bound t1 in
      bind_free_variables1 ctx_free1 ctx_bound t2
  | TcsIf(_,t1,t2,t3) ->
      let ctx_free1 = bind_free_variables1 ctx_free ctx_bound t1 in
      let ctx_free1 = bind_free_variables1 ctx_free1 ctx_bound t2 in
      bind_free_variables1 ctx_free1 ctx_bound t3
  | TcsFix(_,t)
  | TcsFst(_,t)
  | TcsSnd(_,t) ->
      bind_free_variables1 ctx_free ctx_bound t
  | _ -> ctx_free

(* Find unbound variables and add them to the context *)
let bind_free_variables ctx t = 
  bind_free_variables1 ctx emptycontext t

(* Convert term in concrete syntax to abstract syntax *)
let rec convert_term ctx t = match t with
  | TcsVar(fi,x) ->
      let xi = name2index fi ctx x in
      TVar(fi,xi)
  | TcsAbs(fi,x,t) ->
      let ctx1 = addname ctx x in
      let t = convert_term ctx1 t in
      TAbs(fi,x,t)
  | TcsApp(fi,t1,t2) ->
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      TApp(fi,t1,t2)
  | TcsNum(fi,x) ->
      Sugar.num fi x      
  | TcsTrue(fi) ->
      Sugar.ftrue fi
  | TcsFalse(fi) ->
      Sugar.ffalse fi
  | TcsIf(fi,t1,t2,t3) ->
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      let t3 = convert_term ctx t3 in
      TApp(fi, TApp(fi, t1, t2), t3)
  | TcsAdd(fi,t1,t2) ->
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      TApp(fi, TApp(fi, Sugar.add fi, t1), t2)
  | TcsMult(fi,t1,t2) ->
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      TApp(fi, TApp(fi, Sugar.mult fi, t1 ), t2 )
  | TcsFix(fi,t) ->
      let t = convert_term ctx t in
      TApp(fi, Sugar.fix fi, t)
  | TcsPair(fi,t1,t2) ->
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      TApp(fi, TApp(fi, Sugar.pair fi, t1), t2)
  | TcsFst(fi,t) ->
      let t = convert_term ctx t in
      TApp(fi, Sugar.fst fi, t)
  | TcsSnd(fi,t) ->
      let t = convert_term ctx t in
      TApp(fi, Sugar.snd fi, t)
  | TcsNil(fi) ->
      Sugar.nil fi

(* -------------------------  EVALUATION  ------------------------ *)

exception NoRuleApplies

(* Take one step in the CBN reduction *)
let rec step_cbn ctx t =
  match t with 
    TApp(_, TAbs(_, _, t1), t2) ->
      termSubstTop t2 t1
  | TApp(fi, t1, t2) ->
      let t1' = step_cbn ctx t1 in
      TApp(fi, t1', t2)
  | _ ->
      raise NoRuleApplies

(* Evaluate term using CBN strategy *)
let rec eval_cbn ctx t =
  try let t' = step_cbn ctx t
      in eval_cbn ctx t'
  with NoRuleApplies -> t

(* Evaluate to a normal form *)
let rec normalize ctx t =
  let t' = eval_cbn ctx t in match t' with
    TAbs(fi, x, t1) -> 
      let ctx1 = addname ctx x in
      let t1' = normalize ctx1 t1 in
      TAbs(fi, x, t1')
  | TApp(fi, t1, t2) ->
      let t2' = normalize ctx t2 in
      TApp(fi, t1, t2')
  | _ -> t'

(* Check beta-equality of two terms *)
let rec check_equal ctx t1 t2 = 
  let t1_whnf = eval_cbn ctx t1 in
  let t2_whnf = eval_cbn ctx t2 in 
  match (t1_whnf, t2_whnf) with
    (TAbs(_, _, t1), TAbs(_, _, t2)) -> 
      check_equal ctx t1 t2
  | (TApp(_, t11, t12), TApp(_, t21, t22)) ->
      check_equal ctx t11 t21 && check_equal ctx t12 t22
  | (TVar(_, x1), TVar(_, x2)) ->
      x1 == x2
  | _ -> false
