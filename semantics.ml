(* semantics.ml *)

open Ast
open Builtins

type symbol_info = {
  mutable typ : typ option;
  mutable initialized : bool;
}

let rec analyze_program (Program stmts) global_env =
  List.iter (analyze_stmt global_env) stmts

and analyze_stmt env = function
  | LetStmt(name, expr) ->
      let expr_typ = analyze_expr env expr in
      Hashtbl.add env name { typ = Some expr_typ; initialized = true };
      (* print_endline ("[Semantics] Declared variable: " ^ name ^ " with type " ^ string_of_typ expr_typ) *)

  | ExprStmt expr ->
      ignore (analyze_expr env expr)

  | ReturnStmt expr ->
      ignore (analyze_expr env expr)

  | IfStmt(cond, then_stmts, else_stmts) ->
      let cond_typ = analyze_expr env cond in
      if cond_typ <> TBool then
        failwith "[Semantics] Condition of if statement must be a boolean";
      List.iter (analyze_stmt env) then_stmts;
      List.iter (analyze_stmt env) else_stmts

  | WhileStmt(cond, stmts) ->
      let cond_typ = analyze_expr env cond in
      if cond_typ <> TBool then
        failwith "[Semantics] Condition of while loop must be a boolean";
      List.iter (analyze_stmt env) stmts

and analyze_expr env = function
  | Var name ->
      let symbol =
        try Hashtbl.find env name
        with Not_found -> failwith ("[Semantics] Undefined variable: " ^ name)
      in
      if not symbol.initialized then
        failwith ("[Semantics] Uninitialized variable: " ^ name);

      (match symbol.typ with
       | Some t -> t
       | None -> failwith ("[Semantics] No type inferred for variable: " ^ name))

  | BinOp(op, lhs, rhs) ->
      let lhs_typ = analyze_expr env lhs in
      let rhs_typ = analyze_expr env rhs in
      if lhs_typ <> rhs_typ then
        failwith "[Semantics] Type mismatch in binary operation";
      (match op with
       | Add | Sub | Mul | Div | Pow when lhs_typ = TFloat -> lhs_typ
       | And | Or when lhs_typ = TBool -> TBool
       | Eq | Neq | Lt | Gt | Le | Ge -> TBool
       | _ -> failwith "[Semantics] Unsupported binary operation")

  | UnaryOp(op, expr) ->
      let expr_typ = analyze_expr env expr in
      (match op with
       | Neg when expr_typ = TFloat -> expr_typ
       | Not when expr_typ = TBool -> TBool
       | _ -> failwith "[Semantics] Unsupported unary operation")

  | FloatLit _ -> TFloat
  | StringLit _ -> TString
  | BoolLit _ -> TBool

  | FuncCall(name, args) ->
    if is_builtin name then
      let signature = get_builtin_signature name in
      let expected_types = signature.param_types in
      let arg_types = List.map (analyze_expr env) args in

      if List.length arg_types <> List.length expected_types then
        failwith ("[Semantics] Argument count mismatch for builtin function " ^ name);
      List.iter2 (fun expected actual ->
        if expected <> actual then
          failwith ("[Semantics] Type mismatch in argument for builtin function " ^ name
                    ^ ": expected " ^ string_of_typ expected
                    ^ ", got " ^ string_of_typ actual)
      ) expected_types arg_types;
      signature.return_type
    else
      failwith ("[Semantics] Undefined function: " ^ name)
  | Lambda(params, _) ->
      (* Assume lambda returns float*)
      (* Later: analyze return statements *)
      TFunc(List.map (fun _ -> TFloat) params, TFloat)
  (* | Lambda(params, stmts) ->
      let func_env = Hashtbl.create 16 in
      List.iter (fun param ->
        Hashtbl.add func_env param { typ = None; initialized = true }
      ) params;
      List.iter (analyze_stmt func_env) stmts;
      TFunc([], TFloat) *)
  | ArrayLit elems ->
      let elem_types = List.map (analyze_expr env) elems in
      (match elem_types with
      | [] -> failwith "[Semantics] Empty array literal"
      | hd :: tl ->
          if List.for_all ((=) hd) tl then
            TArray hd
          else
            failwith "[Semantics] Inconsistent array element types")
  | DictLit entries ->
      let key_types, value_types = List.split (List.map (fun (k, v) ->
        (analyze_expr env k, analyze_expr env v)) entries)
      in
      (match key_types, value_types with
      | [], [] -> failwith "[Semantics] Empty dictionary literal"
      | k_hd :: k_tl, v_hd :: v_tl ->
          if List.for_all ((=) k_hd) k_tl && List.for_all ((=) v_hd) v_tl then
              TDict (k_hd, v_hd)
          else
              failwith "[Semantics] Inconsistent types in dictionary literal"
      | _ -> failwith "[Semantics] Inconsistent dictionary literal")
  | Index(arr_expr, idx_expr) ->
      let arr_typ = analyze_expr env arr_expr in
      let idx_typ = analyze_expr env idx_expr in
      (match arr_typ with
      | TArray t ->
          if idx_typ <> TFloat then
              failwith "[Semantics] Array index must be a float"
          else t
      | _ -> failwith "[Semantics] Indexing non-array type")