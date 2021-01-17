(* module Syntax

   Defines syntax trees 
*)

open Support.Error

type ty = TyInt | TyBool | TyFunc of ty * ty | TyUnit

type term =
  | TmVar of info * string
  | TmAbs of info * string * ty * term
  | TmLet of info * string * term * term
  | TmApp of info * term * term
  | TmNum of info * int
  | TmFix of info * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmAdd of info * term * term
  | TmSub of info * term * term
  | TmMult of info * term * term
  | TmDiv of info * term * term
  | TmEq of info * term * term
  | TmAnd of info * term * term
  | TmOr of info * term * term
  | TmUnit of info
  | TmException of info * string * ty * term
  | TmThrow of info * string * term * ty
  | TmTry of info * term * (info * string * string * term) list 

type command = Eval of info * term | TypeOf of info * term

type binding = string * ty

type context = { variables : binding list; exceptions : binding list }
