(* repl.ml *)
open Quantex

let semantics_env = Hashtbl.create 32  (* For semantic analysis *)
let global_env = Hashtbl.create 32  (* For interpretation *)

let rec repl () =
  print_string "QuantEx> ";
  let line = read_line () in
  try
    let lexbuf = Lexing.from_string line in
    let ast = Parser.program Lexer.read lexbuf in
    (* Semantic Analysis *)
    Semantics.analyze_program ast semantics_env;
    (* Type Checking *)
    Typechecker.type_check_program ast semantics_env;
    (* Optimization *)
    let optimized_ast = Optimizer.optimize_program ast in
    (* Code Generation *)
    let code = Array.of_list (Codegen.codegen_program optimized_ast) in
    (* Interpretation *)
    let result = Interpreter.run_program code global_env in
    (match result with
     | Some value -> print_endline ("Result: " ^ string_of_value value)
     | None -> print_endline ("No output")); (* No output for statements like `let x = 5;` *)
    repl ()
  with
  | Failure msg -> print_endline ("Error: " ^ msg); repl ()
  | _ -> print_endline "Unknown error"; repl ()

and string_of_value = function
  | VFloat f -> string_of_float f
  | VString s -> s
  | VBool b -> string_of_bool b

let () = repl ()
