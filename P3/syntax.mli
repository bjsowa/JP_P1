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

type type_variable = int

type cty =
  | CtyVar of type_variable
  | CtyFunc of cty * cty
  | CtyUnit
  | CtyProd of cty * cty
  | CtyVoid
  | CtySum of cty * cty

type tyann = Unknown | Type of ty | CType of cty

type idx = ID_1 | ID_2

type term =
  | TmVar of info * tyann * string
  | TmAbs of info * tyann * string * term
  | TmApp of info * tyann * term * term
  | TmUnit of info * tyann
  | TmProd of info * tyann * term * term
  | TmProj of info * tyann * term * idx
  | TmAbort of info * tyann * term
  | TmIn of info * tyann * idx * term
  | TmCase of info * tyann * term * (string * term) * (string * term)

type command = TypeOf of info * term

type binding = string * cty

type context = binding list

type constr = cty * cty

type constraints = constr list