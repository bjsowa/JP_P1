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

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VFunc of (environment * string * term)
  | VFix of value

and environment = (string * value) list

type simple_context =
  | LLet of string * term
  | LApp of term
  | RApp of value
  | LFixApp of value 
  | REnv of environment
  | CFix
  | CIf of term * term
  | LAdd of term
  | RAdd of value
  | LSub of term
  | RSub of value
  | LMult of term
  | RMult of value
  | LDiv of term
  | RDiv of value
  | LEq of term
  | REq of value
  | LAnd of term
  | RAnd of value
  | LOr of term
  | ROr of value
  | CTry
  | LThrow of throw_context

and eval_context = simple_context list

and throw_context = (string * term) * exception_stack * environment * eval_context

(* exception name, (binder, term) *)
and exception_handler = string * (string * term)

and exception_stack = (exception_handler list * environment * eval_context) list
