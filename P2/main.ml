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

let searchpath = ref [ "" ]

let verbose = ref false

let argDefs =
  [
    ( "-I",
      Arg.String (fun f -> searchpath := f :: !searchpath),
      "Append a directory to the search path" );
    ("-v", Arg.Set verbose, "Enable verbose mode");
  ]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
    (fun s ->
      match !inFile with
      | Some _ -> err "You must specify exactly one input file"
      | None -> inFile := Some s)
    "";
  match !inFile with
  | None -> err "You must specify an input file"
  | Some s -> s

let openfile infile =
  let rec trynext l =
    match l with
    | [] -> err ("Could not find " ^ infile)
    | d :: rest -> (
        let name = if d = "" then infile else d ^ "/" ^ infile in
        try open_in name with Sys_error _ -> trynext rest )
  in
  trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile in
  let lexbuf = Lexer.create inFile pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  result

let process_command cmd =
  try
    match cmd with
    | Eval (_, t) ->
        if !verbose then (
          pr "Evaluating: ";
          printtm t;
          pr "\n" );
        let typ = infer_type emptycontext t in
        if !verbose then (
          pr "Inferred type: ";
          printty typ;
          pr "\n" );
        let v = eval t in
        printv v;
        pr "\n"
    | TypeOf (_, t) ->
        if !verbose then (
          pr "Type Checking: ";
          printtm t;
          pr "\n" );
        let typ = infer_type emptycontext t in
        printty typ;
        pr "\n"
  with Exit _ -> ()

let main () =
  let inFile = parseArgs () in
  let cmds = parseFile inFile in
  List.iter process_command cmds

let res =
  Printexc.catch
    (fun () ->
      try
        main ();
        0
      with Exit x -> x)
    ()

let () = print_flush ()

let () = exit res
