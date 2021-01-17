(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context management *)
val emptycontext : context

val lookup : binding list -> string -> ty

val lookup_variable : info -> context -> string -> ty

val add_binding : binding list -> string -> ty -> binding list

val add_variable_binding : context -> string -> ty -> context

(* val add_exception_binding : context -> string -> ty -> context *)

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
val eval_control : term -> eval_context -> value

val eval_kontinuation : value -> eval_context -> value

val eval : term -> value
