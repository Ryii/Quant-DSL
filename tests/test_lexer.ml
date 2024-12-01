(* tests/test_lexer.ml *)
open OUnit2
open Lexer
open Parser

let lex_string s =
  let lexbuf = Lexing.from_string s in
  let rec helper acc =
    let token = Lexer.read lexbuf in
    match token with
    | EOF -> List.rev (EOF :: acc)
    | _ -> helper (token :: acc)
  in
  helper []

let string_of_token = function
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | EOF -> "EOF"
  | _ -> "OTHER"

let test_lexer_simple _ =
  let tokens = lex_string "let x = 5 + 3;" in
  let expected = [LET; IDENT "x"; EQ; FLOAT 5.0; PLUS; FLOAT 3.0; SEMI; EOF] in
  assert_equal ~printer:(fun l -> String.concat " " (List.map string_of_token l)) expected tokens

let test_lexer_float _ =
  let tokens = lex_string "3.1415 * 2.0" in
  let expected = [FLOAT 3.1415; TIMES; FLOAT 2.0; EOF] in
  assert_equal ~printer:(fun l -> String.concat " " (List.map string_of_token l)) expected tokens

let suite =
  "Lexer Tests" >::: [
    "test_lexer_simple" >:: test_lexer_simple;
    "test_lexer_float" >:: test_lexer_float;
  ]

let () = run_test_tt_main suite
