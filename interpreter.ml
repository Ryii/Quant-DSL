(* interpreter.ml *)
open Ast
open Codegen
open Builtins

type env = (string, value) Hashtbl.t

let run_program code env =
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
                      failwith ("Undefined variable: " ^ name ^ ". Available variables: " ^ 
                            (String.concat ", " (Hashtbl.fold (fun k _ acc -> k :: acc) env [])))
              
              in
              Stack.push v stack;
              exec (ip + 1)
      | STORE_VAR name ->
          let v = Stack.pop stack in
          Hashtbl.replace env name v;
          print_endline ("Stored variable: " ^ name ^ " = " ^ string_of_value v);
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
            (* User-defined functions: Currently unimplemented *)
            failwith ("Unknown function: " ^ name)
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
      (* | _ -> failwith "Unsupported opcode" *)
    else
      match Stack.is_empty stack with
      | false -> Some (Stack.pop stack)
      | true -> None
  in
  exec 0

