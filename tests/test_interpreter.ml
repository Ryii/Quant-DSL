open OUnit2
open Quantex.Interpreter
open Quantex.Codegen

let test_interpreter_add _ =
  let env = Hashtbl.create 16 in
  let code = [LOAD_CONST 2.0; LOAD_CONST 3.0; ADD; RETURN] in
  let result = run_program (Array.of_list code) env in
  match result with
  | Some (VFloat res) -> assert_equal 5.0 res ~printer:string_of_float
  | _ -> assert_failure "Addition failed"

let suite =
  "Interpreter Tests" >::: [
    "test_interpreter_add" >:: test_interpreter_add;
  ]

let () =
  run_test_tt_main suite
