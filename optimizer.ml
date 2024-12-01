(* optimizer.ml *)
open Ast

let rec optimize_program (Program(stmts)) =
  Program(List.map optimize_stmt stmts)

and optimize_stmt = function
  | ExprStmt(expr) -> ExprStmt(optimize_expr expr)
  | LetStmt(name, expr) -> LetStmt(name, optimize_expr expr)
  | ReturnStmt(expr) -> ReturnStmt(optimize_expr expr)
  | IfStmt(cond, then_stmts, else_stmts) ->
      let cond' = optimize_expr cond in
      IfStmt(cond', List.map optimize_stmt then_stmts, List.map optimize_stmt else_stmts)
  | WhileStmt(cond, stmts) ->
      let cond' = optimize_expr cond in
      WhileStmt(cond', List.map optimize_stmt stmts)

and optimize_expr = function
  | BinOp(op, FloatLit(a), FloatLit(b)) ->
      let result = match op with
        | Add -> a +. b
        | Sub -> a -. b
        | Mul -> a *. b
        | Div -> a /. b
        | Pow -> a ** b
        | _ -> failwith "Unsupported operation in constant folding"
      in
      FloatLit(result)
  | BinOp(op, lhs, rhs) ->
      BinOp(op, optimize_expr lhs, optimize_expr rhs)
  | UnaryOp(op, expr) ->
      UnaryOp(op, optimize_expr expr)
  | FuncCall(name, args) ->
      FuncCall(name, List.map optimize_expr args)
  | expr -> expr
