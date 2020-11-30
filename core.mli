(* module Core

   Core typechecking and evaluation functions
*)

open Syntax

val eval_cbn : context -> term -> term 
val normalize : context -> term -> term
val check_equal : context -> term -> term -> bool