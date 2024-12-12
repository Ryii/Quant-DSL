(* repl.ml *)
open Quantex

let semantics_env = Hashtbl.create 32
let global_env = Hashtbl.create 32

let run_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let ast = Parser.program Lexer.read lexbuf in
    Semantics.analyze_program ast semantics_env;
    Typechecker.type_check_program ast semantics_env;
    let optimized_ast = Optimizer.optimize_program ast in
    let code = Array.of_list (Codegen.codegen_program optimized_ast) in
    match Interpreter.run_program code global_env with
    | Some value -> print_endline ("Result: " ^ Ast.string_of_value value)
    | None -> print_endline "No result."
  with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | Parsing.Parse_error ->
      print_endline "Error: Syntax error in the script.";
      close_in ic
  | Sys_error msg ->
      print_endline ("Error: Cannot open file: " ^ msg);
      close_in ic
  | e ->
      print_endline ("Unhandled exception: " ^ Printexc.to_string e);
      close_in ic

let rec repl () =
  print_string "QuantEx> ";
  let line = read_line () in
  if String.trim line = "exit" then exit 0 else
    try
      let lexbuf = Lexing.from_string line in
      let ast = Parser.program Lexer.read lexbuf in
      Semantics.analyze_program ast semantics_env;
      Typechecker.type_check_program ast semantics_env;
      let optimized_ast = Optimizer.optimize_program ast in
      let code = Array.of_list (Codegen.codegen_program optimized_ast) in
      match Interpreter.run_program code global_env with
      | Some value -> print_endline ("Result: " ^ Ast.string_of_value value); repl ()
      | None -> repl ()
    with
    | Failure msg -> print_endline ("Error: " ^ msg); repl ()
    | _ -> print_endline "Unknown error"; repl ()

let () =
  let args = Sys.argv in
  if Array.length args > 1 then
    run_file args.(1)
  else
    repl ()
