open OUnit2
open Quantex.Ast

let test_ast_typ _ =
  let expr = BinOp(Add, FloatLit 2.0, FloatLit 3.0) in
  let expected_typ = TFloat in
  let typ = match expr with
    | BinOp(_, FloatLit _, FloatLit _) -> TFloat
    | _ -> failwith "Unexpected type"
  in
  assert_equal expected_typ typ ~printer:string_of_typ

let suite =
  "AST Tests" >::: [
    "test_ast_typ" >:: test_ast_typ;
  ]

let () =
  run_test_tt_main suite
