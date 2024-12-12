(* builtins.ml *)

open Ast

type builtin_signature = {
  param_types: typ list;
  return_type: typ;
}

let builtins = Hashtbl.create 32

let () =
  Hashtbl.add builtins "ln"   { param_types = [TFloat]; return_type = TFloat };
  Hashtbl.add builtins "exp"  { param_types = [TFloat]; return_type = TFloat };
  Hashtbl.add builtins "sqrt" { param_types = [TFloat]; return_type = TFloat };
  Hashtbl.add builtins "N"    { param_types = [TFloat]; return_type = TFloat };

  Hashtbl.add builtins "historical_returns" {
    param_types = [TArray TString; TDict(TString, TString)];
    return_type = TArray (TArray TFloat);
  };
  Hashtbl.add builtins "covariance_matrix"  { 
    param_types = [TArray (TArray TFloat)];
    return_type = TArray (TArray TFloat); 
  };
  Hashtbl.add builtins "mean" {
    param_types = [TArray TFloat];
    return_type = TFloat;
  };
  Hashtbl.add builtins "mean_matrix" {
    param_types = [TArray (TArray TFloat)];
    return_type = TArray TFloat;
  };
  Hashtbl.add builtins "optimize_portfolio" {
    param_types = [TArray TFloat; TArray (TArray TFloat); TDict (TString, TFloat)];
    return_type = TArray TFloat;
  };
  Hashtbl.add builtins "monte_carlo_simulation" { param_types = [TDict(TString, TFloat); TFloat; TFloat]; return_type = TFloat };
  Hashtbl.add builtins "calculate_VaR" { param_types = [TFloat; TFloat]; return_type = TFloat }

let is_builtin name =
  Hashtbl.mem builtins name

let get_builtin_signature name =
  try Hashtbl.find builtins name
  with Not_found -> failwith ("[Builtins] Unknown builtin: " ^ name)

  let solve_linear_system mat vec =
    let n = Array.length mat in
    for i = 0 to n - 1 do
      (* Pivot *)
      let max_row = ref i in
      let max_val = ref (abs_float mat.(i).(i)) in
      for k = i+1 to n - 1 do
        let v = abs_float mat.(k).(i) in
        if v > !max_val then (max_val := v; max_row := k)
      done;
      (* Swap rows i and max_row *)
      if !max_row <> i then
        let tmp = mat.(i) in
        mat.(i) <- mat.(!max_row);
        mat.(!max_row) <- tmp;
        let tmpv = vec.(i) in
        vec.(i) <- vec.(!max_row);
        vec.(!max_row) <- tmpv;
  
      (* Eliminate below i *)
      for k = i+1 to n - 1 do
        let c = mat.(k).(i) /. mat.(i).(i) in
        for j = i to n - 1 do
          mat.(k).(j) <- mat.(k).(j) -. c *. mat.(i).(j)
        done;
        vec.(k) <- vec.(k) -. c *. vec.(i)
      done;
    done;
    
    (* Back substitution *)
    for i = n - 1 downto 0 do
      vec.(i) <- vec.(i) /. mat.(i).(i);
      for k = 0 to i-1 do
        vec.(k) <- vec.(k) -. mat.(k).(i)*.vec.(i)
      done;
    done;
    vec
  
  let normal_cdf x =
    let erf_approx z =
      let t = 1.0 /. (1.0 +. 0.5 *. abs_float z) in
      let tau = t *. exp(-. z *. z -. 1.26551223 +.
                         t *. (1.00002368 +.
                                t *. (0.37409196 +.
                                      t *. (0.09678418 +.
                                            t *. (-0.18628806 +.
                                                  t *. (0.27886807 +.
                                                        t *. (-1.13520398 +.
                                                              t *. (1.48851587 +.
                                                                    t *. (-0.82215223 +.
                                                                          t *. 0.17087277)))))))))
      in if z >= 0.0 then 1.0 -. tau else tau -. 1.0
    in
    0.5 *. (1.0 +. erf_approx (x /. (sqrt 2.0)))
  
    let call_builtin name args =
      match name, args with
      | "ln", [VFloat x] -> VFloat (log x)
      | "exp", [VFloat x] -> VFloat (exp x)
      | "sqrt", [VFloat x] -> VFloat (sqrt x)
      | "N", [VFloat x] -> VFloat (normal_cdf x)
    
      | "historical_returns", [VArray assets; VDict params] ->
          let period =
            match List.assoc_opt (VString "period") params with
            | Some (VString p) -> p
            | _ -> failwith "[Builtins] Missing or invalid period parameter for historical_returns"
          in
          let days =
            match period with
            | "1y" -> 252
            | "6m" -> 126
            | _ -> failwith "[Builtins] Unsupported period"
          in
          let mu = 0.07 /. 252.0 and sigma = 0.15 /. sqrt(252.0) and s0 = 100.0 in
          let simulate_asset () =
            let rec aux prev t acc =
              if t = days then List.rev acc
              else
                let z = Random.float 1.0 in
                let eps = sqrt(-2.0 *. log z) *. cos(2.0 *. Float.pi *. z) in
                let ret = mu +. sigma *. eps in
                let new_price = prev *. exp(ret) in
                let daily_return = (new_price -. prev) /. prev in
                aux new_price (t + 1) (daily_return :: acc)
            in
            aux s0 0 []
          in
          let asset_returns = List.map (fun _ -> simulate_asset ()) assets in
          let transpose matrix =
            let rec loop i acc =
              if i >= days then List.rev acc
              else
                let day_vals = List.map (fun row -> List.nth row i) matrix in
                loop (i + 1) (VArray (List.map (fun x -> VFloat x) day_vals) :: acc)
            in
            loop 0 []
          in
          VArray (transpose asset_returns)
      | "historical_returns", _ -> failwith "[Builtins] Invalid arguments for historical_returns"
    
      | "covariance_matrix", [VArray returns] when returns <> [] ->
          let n = List.length returns in
          let days = match List.hd returns with
            | VArray daily_returns -> List.length daily_returns
            | _ -> failwith "Invalid return structure"
          in
          let data = Array.init n (fun i ->
            match List.nth returns i with
            | VArray daily_returns -> Array.of_list (List.map (function
                | VFloat r -> r
                | _ -> failwith "Invalid return type") daily_returns)
            | _ -> failwith "Invalid return structure"
          ) in
          let means = Array.init n (fun i ->
            Array.fold_left (+.) 0.0 data.(i) /. float_of_int days
          ) in
          let cov i j =
            let sum = ref 0.0 in
            for t = 0 to days - 1 do
              sum := !sum +. (data.(i).(t) -. means.(i)) *. (data.(j).(t) -. means.(j))
            done;
            !sum /. float_of_int (days - 1)
          in
          VArray (List.init n (fun i -> VArray (List.init n (fun j -> VFloat (cov i j)))))
    
      | "covariance_matrix", _ -> failwith "[Builtins] Invalid arguments for covariance_matrix"
    
      | "mean", [VArray elements] ->
        let sum, count =
          List.fold_left
            (fun (acc, cnt) -> function
              | VFloat value -> (acc +. value, cnt + 1)
              | _ -> failwith "[Builtins] Non-float encountered in mean calculation")
            (0.0, 0) elements
        in
        if count = 0 then
          failwith "[Builtins] Cannot calculate mean of an empty array"
        else
          VFloat (sum /. float_of_int count)
          
      | "mean_matrix", [VArray arrays] ->
        let results =
          List.map (function
            | VArray sub_elements ->
                let sum, count =
                  List.fold_left
                    (fun (acc, cnt) -> function
                      | VFloat value -> (acc +. value, cnt + 1)
                      | _ -> failwith "[Builtins] Non-float encountered in sub-array")
                    (0.0, 0) sub_elements
                in
                if count = 0 then
                  failwith "[Builtins] Cannot calculate mean of an empty sub-array"
                else
                  VFloat (sum /. float_of_int count)
            | _ -> failwith "[Builtins] Expected sub-array in mean_matrix calculation")
          arrays
        in
        VArray results
      | "mean", _ -> failwith "[Builtins] Invalid arguments for mean"
    
    
      | "optimize_portfolio", [VArray expected_returns; VArray cov_matrix; VDict params] ->
          if List.length expected_returns = 0 || List.length cov_matrix = 0 then
            failwith "[optimize_portfolio] Empty input arrays";
          let n = List.length expected_returns in
          if List.length cov_matrix <> n then begin
            print_endline (string_of_int (List.length cov_matrix));
            print_endline (string_of_int n);
            failwith "[optimize_portfolio] Dimension mismatch between expected_returns and cov_matrix"
          end;
          let target_return =
            match List.assoc_opt (VString "target_return") params with
            | Some (VFloat t) -> t
            | _ -> failwith "[Builtins] Missing or invalid target_return"
          in
          let n = List.length expected_returns in
          let mu = Array.of_list (List.map (function VFloat r -> r | _ -> failwith "Invalid expected return type") expected_returns) in
          let sigma =
            Array.init n (fun i ->
              if i >= List.length cov_matrix then
                failwith "[optimize_portfolio] Row index out of bounds";
              match List.nth cov_matrix i with
              | VArray row -> Array.of_list (List.map (function VFloat x -> x | _ -> failwith "Invalid covariance matrix element") row)
              | _ -> failwith "Invalid covariance matrix row")
          in
          let big_n = n + 2 in
          let mat = Array.make_matrix big_n big_n 0.0 in
          let vec = Array.make big_n 0.0 in
          for i = 0 to n - 1 do
            for j = 0 to n - 1 do
              mat.(i).(j) <- sigma.(i).(j)
            done;
            mat.(i).(n) <- mu.(i);
            mat.(i).(n + 1) <- 1.0;
            mat.(n).(i) <- mu.(i);
            mat.(n + 1).(i) <- 1.0
          done;
          vec.(n) <- target_return;
          vec.(n + 1) <- 1.0;
          let solution = solve_linear_system mat vec in
          let weights = Array.sub solution 0 n in
          VArray (Array.to_list (Array.map (fun w -> VFloat w) weights))
      | "optimize_portfolio", _ -> failwith "[Builtins] Invalid arguments for optimize_portfolio"
    
      | "monte_carlo_simulation", [VDict portfolio; VFloat num_paths; VFloat horizon] ->
          let mu, sigma = 0.07, 0.15 in
          let initial_value =
            List.fold_left
              (fun acc (k, v) ->
                match (k, v) with
                | VString _, VFloat shares -> acc +. shares *. 100.0
                | _ -> failwith "[Builtins] Invalid portfolio structure")
              0.0 portfolio
          in
          let simulate_path () =
            let dt = horizon /. 252.0 in
            let steps = int_of_float (horizon *. 252.0) in
            let rec aux value step =
              if step >= steps then value
              else
                let eps = Random.float 1.0 in
                aux (value *. exp (mu *. dt +. sigma *. sqrt dt *. eps)) (step + 1)
            in
            aux initial_value 0
          in
          let paths = Array.init (int_of_float num_paths) (fun _ -> simulate_path ()) in
          VFloat (Array.fold_left (+.) 0.0 paths /. num_paths)
      | "monte_carlo_simulation", _ -> failwith "[Builtins] Invalid arguments for monte_carlo_simulation"
    
      | "calculate_VaR", [VFloat portfolio_value; VFloat confidence_level] ->
          let std_dev = portfolio_value *. 0.05 in
          let z =
            if abs_float (confidence_level -. 0.95) < 1e-9 then 1.645
            else if abs_float (confidence_level -. 0.99) < 1e-9 then 2.33
            else failwith "[Builtins] Unsupported confidence level"
          in
          VFloat (-1.0 *. z *. std_dev)
      | "calculate_VaR", [VArray simulations; VFloat confidence_level] ->
          let sorted =
            List.sort compare (List.map (function VFloat x -> x | _ -> failwith "Invalid simulation result") simulations)
          in
          let alpha = 1.0 -. confidence_level in
          let idx = int_of_float (alpha *. float_of_int (List.length sorted)) in
          VFloat (List.nth sorted idx)
      | "calculate_VaR", _ -> failwith "[Builtins] Invalid arguments for calculate_VaR"
    
      | _ -> failwith ("[Builtins] Invalid call or argument mismatch for " ^ name)
    