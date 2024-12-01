open Ast

(* Define the symbol information and type for semantic analysis *)
type symbol_info = {
  mutable typ : typ option;  (* Type of the symbol *)
  mutable initialized : bool;  (* Whether the symbol is initialized *)
}

and typ =
  | TInt
  | TFloat
  | TString
  | TBool
  | TFunc of typ list * typ

(* Analyze the entire program using the provided global environment *)
let rec analyze_program (Program stmts) global_env =
  List.iter (analyze_stmt global_env) stmts

(* Analyze individual statements *)
and analyze_stmt env = function
  | LetStmt(name, expr) ->
      let expr_typ = analyze_expr env expr in
      Hashtbl.add env name { typ = Some expr_typ; initialized = true };
      print_endline ("[Semantics] Declared variable: " ^ name ^ " with type " ^ string_of_typ expr_typ)
  | ExprStmt expr ->
      ignore (analyze_expr env expr)
  | ReturnStmt expr ->
      ignore (analyze_expr env expr)
  | IfStmt(cond, then_stmts, else_stmts) ->
      let cond_typ = analyze_expr env cond in
      if cond_typ <> TBool then
        failwith "Condition of if statement must be a boolean";
      (* No need to create child environments, as we are using the global environment *)
      List.iter (analyze_stmt env) then_stmts;
      List.iter (analyze_stmt env) else_stmts
  | WhileStmt(cond, stmts) ->
      let cond_typ = analyze_expr env cond in
      if cond_typ <> TBool then
        failwith "Condition of while loop must be a boolean";
      List.iter (analyze_stmt env) stmts

(* Analyze expressions and return their types *)
and analyze_expr env = function
  | Var name ->
      let symbol =
        try Hashtbl.find env name
        with Not_found -> failwith ("[Semantics] Undefined variable: " ^ name)
      in
      if not symbol.initialized then
        failwith ("[Semantics] Uninitialized variable: " ^ name);
      Option.get symbol.typ
  | BinOp(op, lhs, rhs) ->
      let lhs_typ = analyze_expr env lhs in
      let rhs_typ = analyze_expr env rhs in
      if lhs_typ <> rhs_typ then
        failwith "Type mismatch in binary operation";
      (match op with
      | Add | Sub | Mul | Div when lhs_typ = TInt || lhs_typ = TFloat -> lhs_typ
      | And | Or when lhs_typ = TBool -> TBool
      | Eq | Neq | Lt | Gt | Le | Ge -> TBool
      | _ -> failwith "Unsupported binary operation")
  | UnaryOp(op, expr) ->
      let expr_typ = analyze_expr env expr in
      (match op with
      | Neg when expr_typ = TInt || expr_typ = TFloat -> expr_typ
      | Not when expr_typ = TBool -> TBool
      | _ -> failwith "Unsupported unary operation")
  | FloatLit _ -> TFloat
  | StringLit _ -> TString
  | BoolLit _ -> TBool
  | FuncCall(name, args) ->
      let symbol =
        try Hashtbl.find env name
        with Not_found -> failwith ("[Semantics] Undefined function: " ^ name)
      in
      (match symbol.typ with
      | Some (TFunc(param_types, return_type)) ->
          if List.length param_types <> List.length args then
            failwith ("Function " ^ name ^ " called with incorrect number of arguments");
          List.iter2 (fun param_typ arg ->
            let arg_typ = analyze_expr env arg in
            if arg_typ <> param_typ then
              failwith ("Type mismatch in argument for function " ^ name)
          ) param_types args;
          return_type
      | _ -> failwith ("Symbol " ^ name ^ " is not a function"))
  | Lambda(params, stmts) ->
      (* For lambdas, a new local environment could be created if needed *)
      let func_env = Hashtbl.create 16 in
      List.iter (fun param ->
        Hashtbl.add func_env param { typ = None; initialized = true }
      ) params;
      List.iter (analyze_stmt func_env) stmts;
      TFunc([], TInt)  (* Update return type as needed *)

(* Helper to convert type to string for debugging *)
and string_of_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TFunc(_, _) -> "function"
