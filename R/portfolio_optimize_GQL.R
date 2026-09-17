#' Optimize a Portfolio
#'
#' Optimize a GaussQuant portfolio specification using asset return data.
#'
#' @param tbl_returns A tibble containing asset returns.
#' @param portfolio A portfolio specification created by `portfolio_spec_GQL()`.
#' @param date_col Name of the date column.
#' @param asset_col Name of the asset column.
#' @param return_col Name of the return column.
#' @param method Portfolio optimization method.
#' @param solver Optimization solver. `"ROI"` dispatches to the appropriate
#'   ROI or nonlinear backend; `"optim"` uses the nonlinear backend.
#' @param risk_aversion Risk-aversion parameter for mean-variance optimization.
#' @param risk_free_rate Per-period risk-free return for maximum Sharpe.
#' @param confidence_level Confidence level for minimum ES.
#' @param target_return Per-period target expected return.
#' @param target_volatility Per-period target volatility.
#' @param risk_budget Optional vector of risk budgets. Defaults to equal budgets.
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
      "maximum_return",
      "maximum_sharpe",
      "minimum_ES",
      "risk_budget",
      "target_return",
      "target_risk"
    ),
    solver = c("ROI", "optim"),
    risk_aversion = 1,
    risk_free_rate = 0,
    confidence_level = 0.95,
    target_return = NULL,
    target_volatility = NULL,
    risk_budget = NULL,
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
    rlang::abort("`risk_aversion` must be a non-negative numeric scalar.")
  }

  if (!is.numeric(risk_free_rate) ||
      length(risk_free_rate) != 1L ||
      !is.finite(risk_free_rate)) {
    rlang::abort("`risk_free_rate` must be a finite numeric scalar.")
  }

  if (!is.numeric(confidence_level) ||
      length(confidence_level) != 1L ||
      !is.finite(confidence_level) ||
      confidence_level <= 0 ||
      confidence_level >= 1) {
    rlang::abort("`confidence_level` must be strictly between 0 and 1.")
  }

  if (method == "target_return" &&
      (is.null(target_return) ||
       !is.numeric(target_return) ||
       length(target_return) != 1L ||
       !is.finite(target_return))) {
    rlang::abort(
      "`target_return` must be a finite numeric scalar for target-return optimization."
    )
  }

  if (method == "target_risk" &&
      (is.null(target_volatility) ||
       !is.numeric(target_volatility) ||
       length(target_volatility) != 1L ||
       !is.finite(target_volatility) ||
       target_volatility <= 0)) {
    rlang::abort(
      "`target_volatility` must be a positive numeric scalar for target-risk optimization."
    )
  }

  n_assets <- nrow(portfolio$assets)

  if (!is.null(risk_budget)) {
    if (!is.numeric(risk_budget) ||
        length(risk_budget) != n_assets ||
        any(!is.finite(risk_budget)) ||
        any(risk_budget < 0) ||
        sum(risk_budget) <= 0) {
      rlang::abort(
        "`risk_budget` must contain one non-negative finite value per asset and have a positive sum."
      )
    }
    risk_budget <- risk_budget / sum(risk_budget)
  }

  market <- portfolio_market_moments_GQH(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    date_col = date_col,
    asset_col = asset_col,
    return_col = return_col
  )

  solver_args <- list(
    portfolio = portfolio,
    mean_returns = market$mean_returns,
    covariance_matrix = market$covariance_matrix,
    matrix_returns = market$matrix_returns,
    method = method,
    risk_aversion = risk_aversion,
    risk_free_rate = risk_free_rate,
    confidence_level = confidence_level,
    target_return = target_return,
    target_volatility = target_volatility,
    risk_budget = risk_budget,
    control = control
  )

  result_solver <- if (solver == "ROI") {
    do.call(portfolio_optimize_ROI_GQH, solver_args)
  } else {
    do.call(portfolio_optimize_optim_GQH, solver_args)
  }

  weights <- result_solver$weights

  statistics <- portfolio_statistics_GQH(
    weights = weights,
    mean_returns = market$mean_returns,
    covariance_matrix = market$covariance_matrix
  )

  sharpe_ratio <- if (statistics$volatility > 0) {
    (statistics$expected_return - risk_free_rate) / statistics$volatility
  } else {
    NA_real_
  }

  portfolio_returns <- as.numeric(
    market$matrix_returns %*% weights
  )

  expected_shortfall <- portfolio_ES_GQH(
    portfolio_returns = portfolio_returns,
    confidence_level = confidence_level
  )

  tbl_weights <- tibble::tibble(
    asset = portfolio$assets$asset,
    weight = as.numeric(weights)
  )

  tbl_statistics <- tibble::tibble(
    metric = c(
      "expected_return",
      "volatility",
      "variance",
      "sharpe_ratio",
      "expected_shortfall"
    ),
    value = c(
      statistics$expected_return,
      statistics$volatility,
      statistics$variance,
      sharpe_ratio,
      expected_shortfall
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
    class = c("portfolio_optimization_GQL", "list")
  )
}
