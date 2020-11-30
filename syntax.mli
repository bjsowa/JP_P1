(* module Syntax

   Defines syntax trees 
*)

open Support.Error

type term_sugar = 
  | TmsVar of info * string
  | TmsAbs of info * string * term_sugar
  | TmsApp of info * term_sugar * term_sugar

type term =
    TmVar of info * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type context = (string) list

type command =
  | Eval of info * term_sugar
  | Equal of info * term_sugar * term_sugar
