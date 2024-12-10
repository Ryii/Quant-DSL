(* codegen.ml *)
open Ast

type opcode =
  | LOAD_CONST of float
  | LOAD_STRING of string
  | LOAD_VAR of string
  | STORE_VAR of string
  | ADD
  | SUB
  | MUL
  | DIV
  | POW
  | NEG
  | CALL_FUNC of string * int
  | RETURN
  | INDEX
  | MAKE_ARRAY of int
  | MAKE_DICT of int
  | NOP

let rec codegen_program (Program(stmts)) =
  let code = ref [] in
  List.iter (fun stmt -> code := !code @ codegen_stmt stmt) stmts;
  !code

and codegen_stmt = function
  | LetStmt(name, expr) ->
      let code = codegen_expr expr in
      code @ [STORE_VAR name]
  | ExprStmt(expr) ->
      codegen_expr expr
  | ReturnStmt(expr) ->
      codegen_expr expr @ [RETURN]
  | _ -> failwith "Unsupported statement in codegen"

and codegen_expr = function
  | FloatLit f -> [LOAD_CONST f]
  | StringLit s -> [LOAD_STRING s]
  | Var name -> [LOAD_VAR name]
  | BinOp(op, lhs, rhs) ->
      let lhs_code = codegen_expr lhs in
      let rhs_code = codegen_expr rhs in
      lhs_code @ rhs_code @ [opcode_of_binop op]
  | UnaryOp(Neg, expr) ->
      codegen_expr expr @ [NEG]
  | FuncCall(name, args) ->
      let args_code = List.flatten (List.map codegen_expr args) in
      args_code @ [CALL_FUNC(name, List.length args)]
  | ArrayLit(elems) ->
      let elems_code = List.flatten (List.map codegen_expr elems) in
      elems_code @ [MAKE_ARRAY (List.length elems)]
  | DictLit pairs ->
      let pairs_code = List.flatten (List.map (fun (k, v) ->
        (codegen_expr k) @ (codegen_expr v)) pairs)
      in
      pairs_code @ [MAKE_DICT (List.length pairs)]
  | Index(arr_expr, idx_expr) ->
      let arr_code = codegen_expr arr_expr in
      let idx_code = codegen_expr idx_expr in
      arr_code @ idx_code @ [INDEX]
  | BoolLit b ->
      let float_val = if b then 1.0 else 0.0 in
      [LOAD_CONST float_val]
  | _ -> failwith "Unsupported expression in codegen"

and opcode_of_binop = function
  | Add -> ADD
  | Sub -> SUB
  | Mul -> MUL
  | Div -> DIV
  | Pow -> POW
  | _ -> failwith "Unsupported binary operation"
