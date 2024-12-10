open OUnit2
open Quantex.Builtins

let test_ln _ =
  let result = call_builtin "ln" [VFloat 2.718281828459] in
  match result with
  | VFloat res -> assert_bool "ln failed" (abs_float (res -. 1.0) < 1e-6)
  | _ -> assert_failure "ln returned wrong type"

let suite =
  "Builtins Tests" >::: [
    "test_ln" >:: test_ln;
  ]

let () =
  run_test_tt_main suite
