(* module Syntax

   Defines syntax trees 
*)

open Support.Error

type term_sugar = 
  | TmsVar of info * string
  | TmsAbs of info * string * term_sugar
  | TmsApp of info * term_sugar * term_sugar
  | TmsNum of info * int
  | TmsTrue of info
  | TmsFalse of info
  | TmsIf of info * term_sugar * term_sugar * term_sugar
  | TmsAdd of info * term_sugar * term_sugar
  | TmsMult of info * term_sugar * term_sugar
  | TmsFix of info * term_sugar
  | TmsPair of info * term_sugar * term_sugar
  | TmsFst of info * term_sugar
  | TmsSnd of info * term_sugar

type term =
    TmVar of info * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type context = (string) list

type command =
  | Eval of info * term_sugar
  | Equal of info * term_sugar * term_sugar
