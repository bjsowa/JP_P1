(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context and environment management *)
val emptycontext : context

val add_binding : context -> string -> ty -> context

val lookup : info -> context -> string -> ty

(* Extracting file info *)
val tmInfo : term -> info

(* Printing *)
val printty : ty -> unit

val printtm : term -> unit
