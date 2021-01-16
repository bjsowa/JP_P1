(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Extracting file info *)
val tmInfo: term -> info
