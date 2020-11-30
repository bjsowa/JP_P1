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
    TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,t2) -> TmAbs(fi,x,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  in walk c t

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d))
    c t

let termShift d t = termShiftAbove d 0 t

let termSubst j s t =
  tmmap
    (fun fi c x n -> if x=j+c then termShift c s else TmVar(fi,x,n))
    0
    t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)
 
(* --------------------  EXTRACTING FILE INFO  ------------------- *)

let tmInfo t = match t with
    TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi, _, _) -> fi 

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
    TmVar(_,_,_) -> true
  | _ -> false

let rec printtm_Term outer ctx t = match t with
    TmAbs(_,x,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
            obox(); pr "lambda "; pr x'; pr ".";
            if (small t2) && not outer then break() else print_space();
            printtm_Term outer ctx' t2;
            cbox())
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(_, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s x -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let isval t = match t with
  | TmVar(_,_,_) -> true
  | _ -> false

let rec eval1_cbn ctx t =
  match t with 
    TmApp(_, TmAbs(_, _, t1), t2) ->
      termSubstTop t2 t1
  | TmApp(fi, v1, t2) when isval v1 ->
      (* pr "DEBUG eval: "; printtm_ATerm true ctx t; force_newline();  *)
      let t2' = eval1_cbn ctx t2 in
      TmApp(fi, v1, t2')  
  | TmApp(fi, t1, t2) ->
      let t1' = eval1_cbn ctx t1 in
      TmApp(fi, t1', t2)
  | _ ->
      raise NoRuleApplies

let rec eval_cbn ctx t =
  try let t' = eval1_cbn ctx t
      in eval_cbn ctx t'
  with NoRuleApplies -> t

let rec normalize ctx t =
  let t' = eval_cbn ctx t in match t' with
    TmAbs(fi, x, t1) -> 
      let ctx1 = addname ctx x in
      let t1' = normalize ctx1 t1 in
      TmAbs(fi, x, t1')
  | TmApp(fi, t1, t2) ->
      let t2' = normalize ctx t2 in
      TmApp(fi, t1, t2')
  | _ -> t'

let rec check_equal ctx t1 t2 = 
  let t1_whnf = eval_cbn ctx t1 in
  let t2_whnf = eval_cbn ctx t2 in 
  match (t1_whnf, t2_whnf) with
    (TmAbs(_, _, t1), TmAbs(_, _, t2)) -> 
      check_equal ctx t1 t2
  | (TmApp(_, t11, t12), TmApp(_, t21, t22)) ->
      check_equal ctx t11 t21 && check_equal ctx t12 t22
  | (TmVar(_, x1, _), TmVar(_, x2, _)) ->
      x1 == x2
  | _ -> false
