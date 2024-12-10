(* typechecker.ml *)

open Ast
open Semantics
open Builtins

let rec type_check_program (Program stmts) env =
  List.iter (type_check_stmt env) stmts

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
      if is_builtin name then
          let signature = get_builtin_signature name in
          if List.length args <> List.length signature.param_types then
              failwith ("[TypeChecker] Builtin function " ^ name ^ " called with incorrect number of arguments");
          List.iter2 (fun param_typ arg ->
              let arg_typ = type_of_expr env arg in
              if arg_typ <> param_typ then
                  failwith ("[TypeChecker] Type mismatch in argument for builtin function " ^ name)
          ) signature.param_types args;
          signature.return_type
      else
          failwith ("[TypeChecker] Undefined function: " ^ name)
  | Lambda(params, stmts) ->
      let func_env = Hashtbl.create 16 in
      List.iter (fun param ->
          Hashtbl.add func_env param { typ = Some TFloat; initialized = true }
      ) params;
      List.iter (type_check_stmt func_env) stmts;
      TFunc(List.map (fun _ -> TFloat) params, TFloat)
  | ArrayLit elems ->
      let elem_types = List.map (type_of_expr env) elems in
      (match elem_types with
      | [] -> failwith "[TypeChecker] Empty array literal"
      | hd :: tl ->
          if List.for_all ((=) hd) tl then
            TArray hd
          else
            failwith "[TypeChecker] Inconsistent array element types")
  | DictLit entries ->
      let key_types, value_types = List.split (List.map (fun (k, v) ->
        (type_of_expr env k, type_of_expr env v)) entries)
      in
      (match key_types, value_types with
        | [], [] -> failwith "[TypeChecker] Empty dictionary literal"
        | k_hd :: k_tl, v_hd :: v_tl ->
            if List.for_all ((=) k_hd) k_tl && List.for_all ((=) v_hd) v_tl then
                TDict (k_hd, v_hd)
            else
                failwith "[TypeChecker] Inconsistent types in dictionary literal"
        | _ -> failwith "[TypeChecker] Inconsistent dictionary literal")
  | Index(arr_expr, idx_expr) ->
      let arr_typ = type_of_expr env arr_expr in
      let idx_typ = type_of_expr env idx_expr in
      (match arr_typ with
      | TArray t ->
          if idx_typ <> TFloat then
              failwith "[TypeChecker] Array index must be a float (acting as an integer)"
          else t
      | _ -> failwith "[TypeChecker] Indexing on non-array type")