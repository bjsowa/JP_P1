(* module Syntax

   Defines syntax trees 
*)

open Support.Error

type term_cs = 
  | TcsVar of info * string
  | TcsAbs of info * string * term_cs
  | TcsApp of info * term_cs * term_cs
  | TcsNum of info * int
  | TcsTrue of info
  | TcsFalse of info
  | TcsIf of info * term_cs * term_cs * term_cs
  | TcsAdd of info * term_cs * term_cs
  | TcsMult of info * term_cs * term_cs
  | TcsSub of info * term_cs * term_cs
  | TcsEq of info * term_cs * term_cs
  | TcsFix of info * term_cs
  | TcsPair of info * term_cs * term_cs
  | TcsFst of info * term_cs
  | TcsSnd of info * term_cs
  | TcsNil of info
  | TcsIsnil of info * term_cs
  | TcsCons of info * term_cs * term_cs
  | TcsHead of info * term_cs
  | TcsTail of info * term_cs

type command =
  | Eval of info * term_cs
  | Equal of info * term_cs * term_cs

type term =
  | TVar of info * int
  | TAbs of info * string * term
  | TApp of info * term * term

type context = (string) list
