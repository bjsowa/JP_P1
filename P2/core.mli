(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* val printty: ty -> unit *)
val printtm: term -> unit

(* Extracting file info *)
val tmInfo: term -> info
