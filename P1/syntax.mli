(* module Syntax

   Defines syntax trees 
*)

open Support.Error

(* terms with syntactic sugar *)
type term_s = 
  | TcsVar of info * string
  | TcsAbs of info * string * term_s
  | TcsApp of info * term_s * term_s
  | TcsNum of info * int
  | TcsTrue of info
  | TcsFalse of info
  | TcsIf of info * term_s * term_s * term_s
  | TcsAdd of info * term_s * term_s
  | TcsMult of info * term_s * term_s
  | TcsSucc of info * term_s
  | TcsPred of info * term_s
  | TcsSub of info * term_s * term_s
  | TcsIszero of info * term_s
  | TcsLeq of info * term_s * term_s
  | TcsEq of info * term_s * term_s
  | TcsFix of info * term_s
  | TcsPair of info * term_s * term_s
  | TcsFst of info * term_s
  | TcsSnd of info * term_s
  | TcsNil of info
  | TcsIsnil of info * term_s
  | TcsCons of info * term_s * term_s
  | TcsHead of info * term_s
  | TcsTail of info * term_s

(* desugared term with De Bruijn indices *)
type term =
  | TVar of info * int
  | TAbs of info * string * term
  | TApp of info * term * term

type command =
  | Eval of info * term_s
  | Equal of info * term_s * term_s

type context = (string) list
