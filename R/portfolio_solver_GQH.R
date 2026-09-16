portfolio_optimize_ROI_GQH <- function(
    portfolio,
    mean_returns,
    covariance_matrix,
    method,
    risk_aversion,
    control = list()
) {
  
  if (!requireNamespace("ROI", quietly = TRUE)) {
    rlang::abort(
      "Package `ROI` is required for solver = \"ROI\"."
    )
  }
  
  n_assets <- nrow(portfolio$assets)
  
  bounds <- portfolio_bounds_GQH(
    portfolio = portfolio
  )
  
  constraint_matrix <- matrix(
    1,
    nrow = 1,
    ncol = n_assets
  )
  
  constraints <- ROI::L_constraint(
    L = constraint_matrix,
    dir = "==",
    rhs = 1
  )
  
  roi_bounds <- ROI::V_bound(
    li = seq_len(n_assets),
    lb = bounds$lower,
    ui = seq_len(n_assets),
    ub = bounds$upper
  )
  
  if (method %in% c(
    "minimum_variance",
    "mean_variance"
  )) {
    
    if (!requireNamespace(
      "ROI.plugin.quadprog",
      quietly = TRUE
    )) {
      rlang::abort(
        paste0(
          "Package `ROI.plugin.quadprog` is required ",
          "for quadratic optimization."
        )
      )
    }
    
    if (method == "minimum_variance") {
      
      Q <- 2 * covariance_matrix
      
      L <- rep(
        0,
        n_assets
      )
      
    } else {
      
      Q <- 2 *
        risk_aversion *
        covariance_matrix
      
      L <- -mean_returns
    }
    
    objective <- ROI::Q_objective(
      Q = Q,
      L = L
    )
    
    op <- ROI::OP(
      objective = objective,
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
    
    if (!requireNamespace(
      "ROI.plugin.glpk",
      quietly = TRUE
    )) {
      rlang::abort(
        paste0(
          "Package `ROI.plugin.glpk` is required ",
          "for linear optimization."
        )
      )
    }
    
    objective <- ROI::L_objective(
      L = -mean_returns
    )
    
    op <- ROI::OP(
      objective = objective,
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
  
  weights <- ROI::solution(
    result
  )
  
  if (length(weights) != n_assets ||
      any(!is.finite(weights))) {
    rlang::abort(
      "ROI failed to return valid portfolio weights."
    )
  }
  
  statistics <- portfolio_statistics_GQH(
    weights = weights,
    mean_returns = mean_returns,
    covariance_matrix = covariance_matrix
  )
  
  objective_value <- switch(
    method,
    
    minimum_variance =
      statistics$variance,
    
    mean_variance =
      risk_aversion *
      statistics$variance -
      statistics$expected_return,
    
    maximum_return =
      -statistics$expected_return
  )
  
  list(
    weights = as.numeric(weights),
    solver = solver_name,
    status = result$status,
    objective_value = objective_value
  )
}


portfolio_optimize_optim_GQH <- function(
    portfolio,
    mean_returns,
    covariance_matrix,
    method,
    risk_aversion,
    control = list()
) {
  
  bounds <- portfolio_bounds_GQH(
    portfolio = portfolio
  )
  
  initial_weights <- portfolio_initial_weights_GQH(
    portfolio = portfolio,
    lower = bounds$lower,
    upper = bounds$upper
  )
  
  objective_function <- function(weights) {
    
    portfolio_objective_value_GQH(
      weights = weights,
      mean_returns = mean_returns,
      covariance_matrix = covariance_matrix,
      method = method,
      risk_aversion = risk_aversion
    )
  }
  
  result <- stats::optim(
    par = initial_weights,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = bounds$lower,
    upper = bounds$upper,
    control = control
  )
  
  weights <- portfolio_normalize_weights_GQH(
    weights = result$par,
    portfolio = portfolio
  )
  
  list(
    weights = weights,
    solver = "stats::optim/L-BFGS-B",
    status = result$convergence,
    objective_value = result$value
  )
}