let assets = ["AAPL", "GOOG", "MSFT"];
let returns = historical_returns(assets, {"period": "1y"});

let cov_matrix = covariance_matrix(returns);
let expected_returns = mean_matrix(returns);

let optimized_weights = optimize_portfolio(
    expected_returns,
    cov_matrix,
    {"target_return": 0.1}
);

optimized_weights;