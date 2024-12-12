
# QuantDSL

QuantDSL is a domain-specific language (DSL) written in OCaml and designed to enable users to perform financial modeling and simulate financial scenarios with ease.

## Features


- **Interactive REPL**: Tests and debug input in real-time according to the following steps:
  -  **Lexing**: Converts the input DSL scripts into tokens using lexical analysis.
  - **Parsing**: Converts tokens into an abstract syntax tree (AST), which represents the program structure.
  - **Semantic Analysis**: Validates variable declarations, types, and function arguments.
  - **Type Checking**: Ensures type consistency across operations.
  - **Optimizing**: Performs constant folding and other optimizations to improve execution efficiency.
  - **Code Generation**: Converts the optimized AST into bytecode instructions for execution.


- **Built-in Financial Functions**:
  - Historical returns simulation (`historical_returns`)
  - Covariance matrix calculation (`covariance_matrix`)
  - Mean and mean matrix computation (`mean`, `mean_matrix`)
  - Value at Risk (VaR) calculation (`calculate_VaR`)
  - Portfolio optimization (`optimize_portfolio`)
  - Monte Carlo simulation (`monte_carlo_simulation`)


- **Customizable Scripts**: Create and run scripts using a simple, intuitive DSL syntax.

## Prerequisites


**OCaml**: Install OCaml and OPAM (OCaml Package Manager).
  ```bash
  sudo apt update
  sudo apt install ocaml opam
  ```

**Dune**: Ensure you have Dune installed for building the project.
  ```bash
  opam install dune
  ```


## Running the DSL



Run the following command to build the project:
```bash
dune build
```

This will generate the executable at:
```plaintext
_build/default/repl.exe
```


### **Interactive Mode**

Launch the REPL to run commands interactively:
```bash
dune exec ./_build/default/repl.exe
```

Example REPL session:
```plaintext
QuantEx> let assets = ["AAPL", "GOOG", "MSFT"];
QuantEx> let returns = historical_returns(assets, {"period": "1y"});
QuantEx> let cov_matrix = covariance_matrix(returns);
QuantEx> let expected_returns = mean_matrix(returns);
QuantEx> let optimized_weights = optimize_portfolio(expected_returns, cov_matrix, {"target_return": 0.1});
QuantEx> optimized_weights;
Result: [0.4, 0.35, 0.25]
```

### **Script Mode**

Run a `.dsl` script file directly:
```bash
dune exec ./_build/default/repl.exe -- path/to/script.dsl
```

#### Example Script: `optimize_weights.dsl`
```dsl
let assets = ["AAPL", "GOOG", "MSFT"];
let returns = historical_returns(assets, {"period":"1y"});
let cov_matrix = covariance_matrix(returns);
let expected_returns = mean_matrix(returns);
let optimized_weights = optimize_portfolio(
    expected_returns,
    cov_matrix,
    {"target_return":0.1}
);
optimized_weights;
```

Run the script:
```bash
dune exec ./_build/default/repl.exe -- optimize_weights.dsl
```

Expected output:
```plaintext
Result: [0.4, 0.35, 0.25]
```

## License

## License

Copyright © 2024 [Ryan Sun] (https://github.com/Ryii).<br />
This project is [MIT](https://github.com/Ryii/Quant-DSL/blob/main/LICENSE.md) licensed.

