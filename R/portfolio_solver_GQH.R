portfolio_optimize_ROI_GQH <- function(
    portfolio,
    mean_returns,
    covariance_matrix,
    matrix_returns,
    method,
    risk_aversion,
    risk_free_rate = 0,
    confidence_level = 0.95,
    target_return = NULL,
    target_volatility = NULL,
    risk_budget = NULL,
    control = list()
) {

  portfolio_assert_core_constraints_GQH(portfolio)

  if (method %in% c(
    "maximum_sharpe",
    "minimum_ES",
    "risk_budget",
    "target_risk"
  )) {
    return(
      portfolio_optimize_optim_GQH(
        portfolio = portfolio,
        mean_returns = mean_returns,
        covariance_matrix = covariance_matrix,
        matrix_returns = matrix_returns,
        method = method,
        risk_aversion = risk_aversion,
        risk_free_rate = risk_free_rate,
        confidence_level = confidence_level,
        target_return = target_return,
        target_volatility = target_volatility,
        risk_budget = risk_budget,
        control = control
      )
    )
  }

  if (!requireNamespace("ROI", quietly = TRUE)) {
    rlang::abort("Package `ROI` is required for solver = \"ROI\".")
  }

  n_assets <- nrow(portfolio$assets)
  bounds <- portfolio_bounds_GQH(portfolio)

  constraint_rows <- list(rep(1, n_assets))
  directions <- "=="
  rhs <- 1

  if (method == "target_return") {
    constraint_rows[[length(constraint_rows) + 1L]] <- mean_returns
    directions <- c(directions, ">=")
    rhs <- c(rhs, target_return)
  }

  constraints <- ROI::L_constraint(
    L = do.call(rbind, constraint_rows),
    dir = directions,
    rhs = rhs
  )

  roi_bounds <- ROI::V_bound(
    li = seq_len(n_assets),
    lb = bounds$lower,
    ui = seq_len(n_assets),
    ub = bounds$upper
  )

  if (method %in% c(
    "minimum_variance",
    "mean_variance",
    "target_return"
  )) {

    if (!requireNamespace("ROI.plugin.quadprog", quietly = TRUE)) {
      rlang::abort(
        "Package `ROI.plugin.quadprog` is required for quadratic optimization."
      )
    }

    if (method == "mean_variance") {
      Q <- 2 * risk_aversion * covariance_matrix
      L <- -mean_returns
    } else {
      Q <- 2 * covariance_matrix
      L <- rep(0, n_assets)
    }

    op <- ROI::OP(
      objective = ROI::Q_objective(Q = Q, L = L),
      constraints = constraints,
      bounds = roi_bounds
    )

    result <- ROI::ROI_solve(
      op,
      solver = "quadprog",
      control = control
    )

    solver_name <- "ROI::quadprog"

  } else if (method == "maximum_return") {

    if (!requireNamespace("ROI.plugin.glpk", quietly = TRUE)) {
      rlang::abort(
        "Package `ROI.plugin.glpk` is required for linear optimization."
      )
    }

    op <- ROI::OP(
      objective = ROI::L_objective(L = -mean_returns),
      constraints = constraints,
      bounds = roi_bounds
    )

    result <- ROI::ROI_solve(
      op,
      solver = "glpk",
      control = control
    )

    solver_name <- "ROI::glpk"

  } else {
    rlang::abort(
      paste0(
        "Method `",
        method,
        "` is not supported by the ROI solver."
      )
    )
  }

  weights <- as.numeric(ROI::solution(result))

  if (length(weights) != n_assets ||
      any(!is.finite(weights))) {
    rlang::abort("ROI failed to return valid portfolio weights.")
  }

  statistics <- portfolio_statistics_GQH(
    weights,
    mean_returns,
    covariance_matrix
  )

  objective_value <- switch(
    method,
    minimum_variance = statistics$variance,
    mean_variance =
      risk_aversion * statistics$variance -
      statistics$expected_return,
    maximum_return = -statistics$expected_return,
    target_return = statistics$variance
  )

  list(
    weights = weights,
    solver = solver_name,
    status = result$status,
    objective_value = objective_value
  )
}


portfolio_optimize_optim_GQH <- function(
    portfolio,
    mean_returns,
    covariance_matrix,
    matrix_returns,
    method,
    risk_aversion,
    risk_free_rate = 0,
    confidence_level = 0.95,
    target_return = NULL,
    target_volatility = NULL,
    risk_budget = NULL,
    control = list()
) {

  portfolio_assert_core_constraints_GQH(portfolio)

  if (!method %in% c(
    "maximum_sharpe",
    "minimum_ES",
    "risk_budget",
    "target_risk"
  )) {
    rlang::abort(
      paste0(
        "Method `",
        method,
        "` is not supported by the nonlinear optimizer."
      )
    )
  }

  bounds <- portfolio_bounds_GQH(portfolio)

  if (any(!is.finite(bounds$lower)) ||
      any(!is.finite(bounds$upper))) {
    rlang::abort(
      "Nonlinear portfolio optimization requires finite box bounds."
    )
  }

  initial_weights <- portfolio_initial_weights_GQH(
    portfolio,
    bounds$lower,
    bounds$upper
  )

  if (is.null(risk_budget)) {
    risk_budget <- rep(
      1 / length(initial_weights),
      length(initial_weights)
    )
  }

  objective_function <- function(raw_weights) {

    weights <- portfolio_project_weights_GQH(
      raw_weights,
      bounds$lower,
      bounds$upper
    )

    statistics <- portfolio_statistics_GQH(
      weights,
      mean_returns,
      covariance_matrix
    )

    if (method == "maximum_sharpe") {

      if (statistics$volatility <= 0) {
        return(1e12)
      }

      value <- -(
        statistics$expected_return -
          risk_free_rate
      ) / statistics$volatility

    } else if (method == "minimum_ES") {

      portfolio_returns <- as.numeric(
        matrix_returns %*% weights
      )

      value <- portfolio_ES_GQH(
        portfolio_returns,
        confidence_level
      )

    } else if (method == "risk_budget") {

      contribution <- portfolio_risk_contribution_GQH(
        weights,
        covariance_matrix
      )

      if (any(!is.finite(contribution))) {
        return(1e12)
      }

      value <- sum(
        (contribution - risk_budget)^2
      )

    } else {

      value <- -statistics$expected_return

      value <- value +
        portfolio_penalty_GQH(
          weights = weights,
          lower = bounds$lower,
          upper = bounds$upper,
          target_volatility = target_volatility,
          covariance_matrix = covariance_matrix
        )
    }

    value
  }

  result <- stats::optim(
    par = initial_weights,
    fn = objective_function,
    method = "Nelder-Mead",
    control = control
  )

  weights <- portfolio_project_weights_GQH(
    result$par,
    bounds$lower,
    bounds$upper
  )

  statistics <- portfolio_statistics_GQH(
    weights,
    mean_returns,
    covariance_matrix
  )

  objective_value <- if (method == "maximum_sharpe") {
    -(
      statistics$expected_return -
        risk_free_rate
    ) / statistics$volatility
  } else if (method == "minimum_ES") {
    portfolio_ES_GQH(
      as.numeric(matrix_returns %*% weights),
      confidence_level
    )
  } else if (method == "risk_budget") {
    contribution <- portfolio_risk_contribution_GQH(
      weights,
      covariance_matrix
    )
    sum((contribution - risk_budget)^2)
  } else {
    -statistics$expected_return
  }

  list(
    weights = weights,
    solver = "stats::optim/Nelder-Mead",
    status = list(
      code = result$convergence,
      message = result$message
    ),
    objective_value = objective_value
  )
}
