open Ast
open Semantics

(* Perform type checking for the entire program *)
let rec type_check_program (Program stmts) env =
  List.iter (type_check_stmt env) stmts

(* Type check individual statements *)
and type_check_stmt env = function
  | LetStmt(name, expr) ->
      let expr_typ = type_of_expr env expr in
      Hashtbl.add env name { typ = Some expr_typ; initialized = true };
      print_endline ("[TypeChecker] Declared variable: " ^ name ^ " with type " ^ string_of_typ expr_typ)
  | ExprStmt expr ->
      ignore (type_of_expr env expr)
  | ReturnStmt expr ->
      ignore (type_of_expr env expr)
  | IfStmt(cond, then_stmts, else_stmts) ->
      let cond_typ = type_of_expr env cond in
      if cond_typ <> TBool then failwith "[TypeChecker] Condition must be a boolean";
      List.iter (type_check_stmt env) then_stmts;
      List.iter (type_check_stmt env) else_stmts
  | WhileStmt(cond, stmts) ->
      let cond_typ = type_of_expr env cond in
      if cond_typ <> TBool then failwith "[TypeChecker] Condition must be a boolean";
      List.iter (type_check_stmt env) stmts

(* Determine the type of an expression *)
and type_of_expr env = function
  | FloatLit _ -> TFloat
  | StringLit _ -> TString
  | BoolLit _ -> TBool
  | Var name ->
      (match Hashtbl.find_opt env name with
      | Some symbol ->
          if not symbol.initialized then failwith ("[TypeChecker] Variable not initialized: " ^ name);
          Option.get symbol.typ
      | None -> failwith ("[TypeChecker] Unbound variable: " ^ name))
  | BinOp(op, lhs, rhs) ->
      let lhs_typ = type_of_expr env lhs in
      let rhs_typ = type_of_expr env rhs in
      (match op, lhs_typ, rhs_typ with
      | (Add | Sub | Mul | Div | Pow), TFloat, TFloat -> TFloat
      | (Eq | Neq | Lt | Gt | Le | Ge), TFloat, TFloat -> TBool
      | (And | Or), TBool, TBool -> TBool
      | _ -> failwith "[TypeChecker] Type error in binary operation")
  | UnaryOp(op, expr) ->
      let expr_typ = type_of_expr env expr in
      (match op, expr_typ with
      | Neg, TFloat -> TFloat
      | Not, TBool -> TBool
      | _ -> failwith "[TypeChecker] Type error in unary operation")
  | FuncCall(name, args) ->
      (match Hashtbl.find_opt env name with
      | Some { typ = Some (TFunc(param_types, return_type)); _ } ->
          if List.length param_types <> List.length args then
            failwith ("[TypeChecker] Function " ^ name ^ " called with incorrect number of arguments");
          List.iter2 (fun param_typ arg ->
            let arg_typ = type_of_expr env arg in
            if arg_typ <> param_typ then
              failwith ("[TypeChecker] Type mismatch in argument for function " ^ name)
          ) param_types args;
          return_type
      | Some _ -> failwith ("[TypeChecker] Symbol " ^ name ^ " is not a function")
      | None -> failwith ("[TypeChecker] Undefined function: " ^ name))
  | Lambda(params, stmts) ->
      let func_env = Hashtbl.create 16 in
      List.iter (fun param ->
        Hashtbl.add func_env param { typ = Some TFloat; initialized = true }
      ) params;
      List.iter (type_check_stmt func_env) stmts;
      TFunc(List.map (fun _ -> TFloat) params, TFloat)

(* Convert type to string for debugging *)
and string_of_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TFunc(_, _) -> "function"
