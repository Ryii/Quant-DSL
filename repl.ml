(* repl.ml *)
open Quantex

let semantics_env = Hashtbl.create 32
let global_env = Hashtbl.create 32

let rec repl () =
  print_string "QuantEx> ";
  let line = read_line () in
  try
    let lexbuf = Lexing.from_string line in
    let ast = Parser.program Lexer.read lexbuf in
    Semantics.analyze_program ast semantics_env;
    Typechecker.type_check_program ast semantics_env;
    let optimized_ast = Optimizer.optimize_program ast in
    let code = Array.of_list (Codegen.codegen_program optimized_ast) in
    let result = Interpreter.run_program code global_env in
    let is_expression input =
      try
        let lexbuf = Lexing.from_string input in
        match Parser.program Lexer.read lexbuf with
        | Program [ExprStmt _] -> true
        | _ -> false
      with _ -> false
    in
    match result with
     | Some value -> print_endline ("Result: " ^ Ast.string_of_value value); repl ()
     | None -> if is_expression line then print_endline line; repl ()
  with
  | Failure msg -> print_endline ("Error: " ^ msg); repl ()
  | _ -> print_endline "Unknown error"; repl ()

let () = repl ()
