(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context and environment management *)
val emptycontext : context

val add_binding : context -> string -> cty -> context

val lookup : info -> context -> string -> cty

(* Extracting file info *)
val tmInfo : term -> info

(* Printing *)  
val printcty : cty -> unit

val printconstr : constr -> unit

val printtm : term -> unit

(* Type Checking *)
val fresh_type_variable : unit -> cty

val infer_type : context -> term -> (cty * constraints)