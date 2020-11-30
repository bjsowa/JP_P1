open Format
open Syntax
open Support.Pervasive

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
