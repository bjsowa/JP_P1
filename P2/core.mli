(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context and environment management *)
val emptycontext : context

val lookup_binding : binding list -> string -> ty

val lookup_variable_type : info -> context -> string -> ty

val lookup_exception_type : info -> context -> string -> ty

val lookup_variable : info -> environment -> string -> value

val lookup_exception : info -> exception_stack -> string -> throw_context

val add_binding : binding list -> string -> ty -> binding list

val add_variable_binding : context -> string -> ty -> context

val add_exception_binding : context -> string -> ty -> context

(* Extracting file info *)
val tmInfo : term -> info

(* Printing *)
val printty : ty -> unit

val printtm : term -> unit

val printv : value -> unit

(* Type checking *)
val infer_type : context -> term -> ty

val check_type : context -> term -> ty -> bool

(* Evaluation *)
val eval_control :
  exception_stack -> environment -> eval_context -> term -> value

val eval_kontinuation :
  exception_stack -> environment -> eval_context -> value -> value

val eval : term -> value
