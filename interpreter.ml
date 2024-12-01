(* interpreter.ml *)
open Codegen

type value =
  | VFloat of float
  | VString of string
  | VBool of bool

type env = (string, value) Hashtbl.t

let run_program code env =
  let stack = Stack.create () in
  let rec exec ip =
    if ip < Array.length code then
      match code.(ip) with
      | LOAD_CONST f ->
          Stack.push (VFloat f) stack;
          exec (ip + 1)
      | LOAD_VAR name ->
        print_endline ("Loaded variable: " ^ name);
            let v =
                try Hashtbl.find env name
                with Not_found -> failwith ("Undefined variable: " ^ name)
            in
            print_endline ("Loaded variable: " ^ name ^ " = " ^ string_of_value v);
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
      | _ -> failwith "Unsupported opcode"
    else
        None (* No RETURN was encountered *)
  and string_of_value = function
    | VFloat f -> string_of_float f
    | VString s -> s
    | VBool b -> string_of_bool b
  in
  exec 0

