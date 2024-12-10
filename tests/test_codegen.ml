open OUnit2
open Quantex.Codegen
open Quantex.Ast

let test_codegen_simple_expr _ =
  let expr = BinOp(Add, FloatLit 2.0, FloatLit 3.0) in
  let code = codegen_expr expr in
  let expected = [LOAD_CONST 2.0; LOAD_CONST 3.0; ADD] in
  assert_equal expected code

let suite =
  "Codegen Tests" >::: [
    "test_codegen_simple_expr" >:: test_codegen_simple_expr;
  ]

let () =
  run_test_tt_main suite