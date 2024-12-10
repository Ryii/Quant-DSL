open OUnit2
open Quantex.Semantics
open Quantex.Ast

let test_simple_let _ =
  let env = Hashtbl.create 16 in
  let stmt = LetStmt("x", FloatLit 3.0) in
  analyze_stmt env stmt;
  match Hashtbl.find_opt env "x" with
  | Some { typ = Some TFloat; initialized = true } -> ()
  | _ -> assert_failure "LetStmt analysis failed"

let test_type_mismatch _ =
  let env = Hashtbl.create 16 in
  let expr = BinOp(Add, FloatLit 3.0, StringLit "hello") in
  assert_raises (Failure "[Semantics] Type mismatch in binary operation")
    (fun () -> ignore (analyze_expr env expr))

let suite =
  "Semantics Tests" >::: [
    "test_simple_let" >:: test_simple_let;
    "test_type_mismatch" >:: test_type_mismatch;
  ]

let () =
  run_test_tt_main suite
