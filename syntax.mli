(* module Syntax

   Defines syntax trees 
*)

open Support.Error

(* type term_sugar = 
  | Var of info * string
  | Abs of info * string * term_sugar
  | App of   *)

type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type context = (string) list

type command =
  | Eval of info * term
  | Equal of info * term * term
