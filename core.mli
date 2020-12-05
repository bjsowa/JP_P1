(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

(* Context management *)
val emptycontext : context 
val ctxlength : context -> int
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val pickfreshname : context -> string -> (context * string)

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
val printtm: context -> term -> unit

(* Extracting file info *)
val tmInfo: term -> info
val tmsInfo: term_cs -> info

(* Converting terms to abstract syntax *)
val bind_free_variables: context -> term_cs -> context
val convert_term: context -> term_cs -> term

(* Evaluation *)
val eval_cbn : context -> term -> term 
val normalize : context -> term -> term
val check_equal : context -> term -> term -> bool