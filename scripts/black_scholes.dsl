let S = 100.0;        // Underlying asset price
let K = 100.0;        // Strike price
let T = 1.0;          // Time to maturity in years
let r = 0.05;         // Risk-free interest rate
let sigma = 0.2;      // Volatility

let d1 = (ln(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T));
let d2 = d1 - sigma * sqrt(T);

let call_price = S * N(d1) - K * exp(-r * T) * N(d2);
call_price;