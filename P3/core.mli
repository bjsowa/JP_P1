(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context and environment management *)
val emptycontext : context

val add_binding : context -> string -> cty -> context

val lookup : info -> context -> string -> cty

(* Extracting info *)
val tmInfo : term -> info

val tmTyann : term -> tyann

(* Printing *)
val printty : ty -> unit

val printcty : cty -> unit

val printconstr : constr -> unit

val printtm : term -> unit

val printtyann : tyann -> unit

val printtm_ann : term -> unit

(* Type Checking *)
val type_counter : int ref

val reset_type_counter : unit -> unit

val fresh_type_variable : unit -> cty

(* The returned term contains type annotations *)
val infer_type : context -> term -> term * cty * constraints

val process_constraints : constraints -> unification_array -> unit

val unify : term -> constraints -> term
