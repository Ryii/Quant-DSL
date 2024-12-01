(* codegen.ml *)
open Ast

(* For simplicity, we'll define a simple virtual machine code *)

type opcode =
  | LOAD_CONST of float
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
  | JUMP_IF_FALSE of int
  | JUMP of int
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
      codegen_expr expr @ [NOP]
  | ReturnStmt(expr) ->
      codegen_expr expr @ [RETURN]
  | _ -> failwith "Unsupported statement in codegen"

and codegen_expr = function
  | FloatLit(f) -> [LOAD_CONST f]
  | Var(name) -> [LOAD_VAR name]
  | BinOp(op, lhs, rhs) ->
      let lhs_code = codegen_expr lhs in
      let rhs_code = codegen_expr rhs in
      lhs_code @ rhs_code @ [opcode_of_binop op]
  | UnaryOp(Neg, expr) ->
      codegen_expr expr @ [NEG]
  | FuncCall(name, args) ->
      let args_code = List.flatten (List.map codegen_expr args) in
      args_code @ [CALL_FUNC(name, List.length args)]
  | _ -> failwith "Unsupported expression in codegen"

and opcode_of_binop = function
  | Add -> ADD
  | Sub -> SUB
  | Mul -> MUL
  | Div -> DIV
  | Pow -> POW
  | _ -> failwith "Unsupported binary operation"
