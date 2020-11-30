(* module Syntax

   Defines syntax trees 
*)

open Support.Error

(* type term_sugar = 
  | Var of info * string
  | Abs of info * string * term_sugar
  | App of   *)

(* Data type definitions *)
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type binding =
    NameBind 
    
type context = (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding
  | Equal of info * term * term
