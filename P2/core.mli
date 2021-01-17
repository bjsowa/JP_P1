(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context management *)
val emptycontext : context 

(* Extracting file info *)
val tmInfo: term -> info

(* Printing *)
val printty: ty -> unit
val printtm: term -> unit

(* Type checking *)
val lookup: context -> string -> ty
val infer_type: context -> term -> ty
val check_type: context -> term -> ty -> bool