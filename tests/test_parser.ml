(* tests/test_parser.ml *)
open OUnit2
open Parser
open Lexer
open Ast

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.read lexbuf

let test_parser_let _ =
  let ast = parse_string "let x = 5 + 3;" in
  match ast with
  | Program([LetStmt("x", BinOp(Add, FloatLit 5.0, FloatLit 3.0))]) -> ()
  | _ -> assert_failure "Parsing 'let x = 5 + 3;' failed"

let test_parser_expr_stmt _ =
  let ast = parse_string "x + y;" in
  match ast with
  | Program([ExprStmt(BinOp(Add, Var "x", Var "y"))]) -> ()
  | _ -> assert_failure "Parsing 'x + y;' failed"

let suite =
  "Parser Tests" >::: [
    "test_parser_let" >:: test_parser_let;
    "test_parser_expr_stmt" >:: test_parser_expr_stmt;
  ]

let () = run_test_tt_main suite
