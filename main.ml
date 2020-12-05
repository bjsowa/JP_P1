(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error _ -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let process_command cmd = match cmd with
  | Eval(_,t) -> 
      let ctx = bind_free_variables emptycontext t in
      let t = convert_term ctx t in
      pr "Evaluating: "; printtm ctx t; force_newline();
      (* pr "Context: "; List.iter (fun s -> pr s; pr ", ") ctx; force_newline(); *)
      let t = normalize ctx t in 
      printtm ctx t; force_newline();
  | Equal(_, t1, t2) ->
      let ctx = bind_free_variables emptycontext t1 in
      let ctx = bind_free_variables ctx t2 in
      let t1 = convert_term ctx t1 in
      let t2 = convert_term ctx t2 in
      pr "Checking for equality:"; force_newline();
      pr "T1: "; printtm ctx t1; force_newline();
      pr "T2: "; printtm ctx t2; force_newline();
      printf "Answer: %b" (check_equal ctx t1 t2); force_newline()
  
let main () = 
  let inFile = parseArgs() in
  let cmds = parseFile inFile in
  List.iter process_command cmds

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
