#' Optimize a Portfolio
#'
#' Optimize a GaussQuant portfolio specification using asset return data.
#'
#' @param tbl_returns A tibble containing asset returns.
#' @param portfolio A portfolio specification created by
#'   `portfolio_spec_GQL()`.
#' @param date_col Name of the date column.
#' @param asset_col Name of the asset column.
#' @param return_col Name of the return column.
#' @param method Portfolio optimization method.
#' @param solver Optimization solver.
#' @param risk_aversion Risk-aversion parameter used by mean-variance
#'   optimization.
#' @param control Optional solver control list.
#'
#' @return An object of class `portfolio_optimization_GQL`.
#'
#' @export
portfolio_optimize_GQL <- function(
    tbl_returns,
    portfolio,
    date_col = "date",
    asset_col = "asset",
    return_col = "return",
    method = c(
      "minimum_variance",
      "mean_variance",
      "maximum_return"
    ),
    solver = c(
      "ROI",
      "optim"
    ),
    risk_aversion = 1,
    control = list()
) {
  
  validate_portfolio_GQH(portfolio)
  
  method <- rlang::arg_match(method)
  solver <- rlang::arg_match(solver)
  
  validate_returns_GQH(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    date_col = date_col,
    asset_col = asset_col,
    return_col = return_col
  )
  
  if (!is.numeric(risk_aversion) ||
      length(risk_aversion) != 1L ||
      !is.finite(risk_aversion) ||
      risk_aversion < 0) {
    rlang::abort(
      "`risk_aversion` must be a non-negative numeric scalar."
    )
  }
  
  market <- portfolio_market_moments_GQH(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    date_col = date_col,
    asset_col = asset_col,
    return_col = return_col
  )
  
  result_solver <- switch(
    solver,
    
    ROI = portfolio_optimize_ROI_GQH(
      portfolio = portfolio,
      mean_returns = market$mean_returns,
      covariance_matrix = market$covariance_matrix,
      method = method,
      risk_aversion = risk_aversion,
      control = control
    ),
    
    optim = portfolio_optimize_optim_GQH(
      portfolio = portfolio,
      mean_returns = market$mean_returns,
      covariance_matrix = market$covariance_matrix,
      method = method,
      risk_aversion = risk_aversion,
      control = control
    )
  )
  
  weights <- result_solver$weights
  
  statistics <- portfolio_statistics_GQH(
    weights = weights,
    mean_returns = market$mean_returns,
    covariance_matrix = market$covariance_matrix
  )
  
  tbl_weights <- tibble::tibble(
    asset = portfolio$assets$asset,
    weight = as.numeric(weights)
  )
  
  tbl_statistics <- tibble::tibble(
    metric = c(
      "expected_return",
      "volatility",
      "variance"
    ),
    value = c(
      statistics$expected_return,
      statistics$volatility,
      statistics$variance
    )
  )
  
  structure(
    list(
      weights = tbl_weights,
      statistics = tbl_statistics,
      constraints = portfolio$constraints,
      objectives = portfolio$objectives,
      method = method,
      solver = result_solver$solver,
      status = result_solver$status,
      objective_value = result_solver$objective_value,
      observations = market$observations
    ),
    class = c(
      "portfolio_optimization_GQL",
      "list"
    )
  )
}