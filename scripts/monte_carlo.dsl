let portfolio = {"AAPL": 100,
    "TSLA": 50,
    "AMZN": 30
};

let simulations = monte_carlo_simulation(portfolio, 10000, 1); // portfolio, num_paths, horizon

let VaR_95 = calculate_VaR(simulations, 0.95); // simulations, confidence
VaR_95;