open Ast
open Codegen
open Builtins

type env = (string, value) Hashtbl.t

let clone_env original_env =
  let new_env = Hashtbl.create (Hashtbl.length original_env) in
  Hashtbl.iter (fun k v -> Hashtbl.add new_env k v) original_env;
  new_env

let rec run_statements code env =
  let code_arr = Array.of_list code in
  run_program code_arr env

and run_program code env =
  let stack = Stack.create () in
  let rec exec ip =
    if ip < Array.length code then
      match code.(ip) with
      | LOAD_CONST f ->
          Stack.push (VFloat f) stack;
          exec (ip + 1)
      | LOAD_STRING s ->
          Stack.push (VString s) stack;
          exec (ip + 1)
      | LOAD_VAR name ->
          let v =
            try Hashtbl.find env name
            with Not_found ->
              failwith ("Undefined variable: " ^ name)
          in
          Stack.push v stack;
          exec (ip + 1)
      | STORE_VAR name ->
          let v = Stack.pop stack in
          Hashtbl.replace env name v;
          (* print_endline ("Stored variable: " ^ name ^ " = " ^ string_of_value v); *)
          exec (ip + 1)
      | ADD ->
          (match Stack.pop stack, Stack.pop stack with
          | VFloat b, VFloat a ->
              Stack.push (VFloat (a +. b)) stack
          | _ -> failwith "ADD: operands must be numbers");
          exec (ip + 1)
      | SUB ->
          (match Stack.pop stack, Stack.pop stack with
          | VFloat b, VFloat a ->
              Stack.push (VFloat (a -. b)) stack
          | _ -> failwith "SUB: operands must be numbers");
          exec (ip + 1)
      | MUL ->
          (match Stack.pop stack, Stack.pop stack with
          | VFloat b, VFloat a ->
              Stack.push (VFloat (a *. b)) stack
          | _ -> failwith "MUL: operands must be numbers");
          exec (ip + 1)
      | DIV ->
          (match Stack.pop stack, Stack.pop stack with
          | VFloat b, VFloat a ->
              Stack.push (VFloat (a /. b)) stack
          | _ -> failwith "DIV: operands must be numbers");
          exec (ip + 1)
      | POW ->
          (match Stack.pop stack, Stack.pop stack with
          | VFloat b, VFloat a ->
              Stack.push (VFloat (a ** b)) stack
          | _ -> failwith "POW: operands must be numbers");
          exec (ip + 1)
      | NEG ->
          (match Stack.pop stack with
          | VFloat a ->
              Stack.push (VFloat (~-. a)) stack
          | _ -> failwith "NEG: operand must be a number");
          exec (ip + 1)
      | RETURN ->
          Some (Stack.pop stack)
      | NOP ->
          exec (ip + 1)
      | CALL_FUNC(name, argc) ->
        let args = List.init argc (fun _ -> Stack.pop stack) |> List.rev in
        if is_builtin name then
          let v = call_builtin name args in
          Stack.push v stack;
          exec (ip + 1)
        else
          (* Check if this is a user-defined function *)
          (match Hashtbl.find_opt env name with
          | Some (VClosure(params, body_stmts, closure_env)) ->
              if List.length params <> List.length args then
                failwith ("Incorrect number of arguments for function: " ^ name);
              let new_env = clone_env closure_env in
              List.iter2 (fun p a -> Hashtbl.replace new_env p a) params args;
              let code = codegen_program (Program body_stmts) in
              (match run_program (Array.of_list code) new_env with
              | Some v -> Stack.push v stack; exec (ip + 1)
              | None -> Stack.push (VFloat 0.0) stack; exec (ip + 1))
          | _ ->
              failwith ("Unknown function: " ^ name))
      | MAKE_ARRAY n ->
          let arr_elems = List.init n (fun _ -> Stack.pop stack) |> List.rev in
          Stack.push (VArray arr_elems) stack;
          exec (ip + 1)
      | MAKE_DICT n ->
          let rec pop_pairs acc count =
            if count = 0 then acc else
            let v = Stack.pop stack in
            let k = Stack.pop stack in
            pop_pairs ((k, v)::acc) (count - 1)
          in
          let pairs = pop_pairs [] n in
          Stack.push (VDict pairs) stack;
          exec (ip + 1)
      | INDEX ->
          let idx_val = Stack.pop stack in
          let container_val = Stack.pop stack in
          (match container_val with
          | VArray elems ->
              (match idx_val with
                | VFloat f_idx ->
                    let i = int_of_float f_idx in
                    if i < 0 || i >= List.length elems then
                      failwith ("Array index out of bounds: " ^ string_of_int i);
                    Stack.push (List.nth elems i) stack;
                    exec (ip + 1)
                | _ -> failwith "INDEX: array index must be a float")
          | VDict pairs ->
              let rec find_key k = function
                | [] -> failwith "INDEX: key not found in dictionary"
                | (kk, vv)::rest ->
                    if kk = idx_val then vv else find_key k rest
              in
              let value = find_key idx_val pairs in
              Stack.push value stack;
              exec (ip + 1)
          | _ -> failwith "INDEX: can only index arrays or dictionaries")
      | LOAD_CLOSURE(params, stmts) ->
          let closure_env = clone_env env in
          Stack.push (VClosure(params, stmts, closure_env)) stack;
          exec (ip + 1)
    else
      match Stack.is_empty stack with
      | false -> Some (Stack.pop stack)
      | true -> None
  in
  exec 0