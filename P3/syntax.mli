(* module Syntax

   Defines syntax trees 
*)

open Support.Error

type ty =
  | TyFunc of ty * ty
  | TyUnit
  | TyProd of ty * ty
  | TyVoid
  | TySum of ty * ty

type idx = ID_1 | ID_2

type term =
  | TmVar of info * string
  | TmAbs of info * string * term
  | TmApp of info * term * term
  | TmUnit of info
  | TmProd of info * term * term
  | TmProj of info * term * idx
  | TmAbort of info * term
  | TmIn of info * idx * term
  | TmCase of info * term * (string * term) * (string * term)

type command = TypeOf of info * term

type binding = string * ty

type context = binding list
