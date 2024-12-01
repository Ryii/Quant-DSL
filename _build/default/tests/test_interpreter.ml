(* tests/test_interpreter.ml *)
open OUnit2
open Lexer
open Parser
open Interpreter
open Codegen
open Semantics
open Typechecker
open Optimizer
open Ast

let interpret_string s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  Semantics.analyze_program ast;
  Typechecker.type_check_program ast;
  let optimized_ast = Optimizer.optimize_program ast in
  let code = Array.of_list (Codegen.codegen_program optimized_ast) in
  Interpreter.run_program code

let test_interpreter_addition _ =
  let result = interpret_string "5 + 3;" in
  match result with
  | VFloat f -> assert_equal 8.0 f
  | _ -> assert_failure "Interpreter failed on '5 + 3;'"

let test_interpreter_let _ =
  let result = interpret_string "let x = 10; x * 2;" in
  match result with
  | VFloat f -> assert_equal 20.0 f
  | _ -> assert_failure "Interpreter failed on 'let x = 10; x * 2;'"

let suite =
  "Interpreter Tests" >::: [
    "test_interpreter_addition" >:: test_interpreter_addition;
    "test_interpreter_let" >:: test_interpreter_let;
  ]

let () = run_test_tt_main suite
