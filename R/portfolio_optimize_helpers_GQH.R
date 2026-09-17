validate_returns_GQH <- function(
    tbl_returns,
    portfolio,
    date_col,
    asset_col,
    return_col
) {

  if (!inherits(tbl_returns, "data.frame")) {
    rlang::abort("`tbl_returns` must be a data frame or tibble.")
  }

  required_columns <- c(date_col, asset_col, return_col)
  missing_columns <- setdiff(required_columns, names(tbl_returns))

  if (length(missing_columns) > 0L) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      )
    )
  }

  assets_data <- unique(as.character(tbl_returns[[asset_col]]))
  missing_assets <- setdiff(portfolio$assets$asset, assets_data)

  if (length(missing_assets) > 0L) {
    rlang::abort(
      paste0(
        "Return data are missing portfolio assets: ",
        paste(missing_assets, collapse = ", "),
        "."
      )
    )
  }

  returns <- tbl_returns[[return_col]]

  if (!is.numeric(returns)) {
    rlang::abort("The return column must be numeric.")
  }

  invisible(TRUE)
}


portfolio_market_moments_GQH <- function(
    tbl_returns,
    portfolio,
    date_col,
    asset_col,
    return_col
) {

  assets <- portfolio$assets$asset

  tbl_filtered <- tbl_returns |>
    dplyr::filter(.data[[asset_col]] %in% assets) |>
    dplyr::select(
      dplyr::all_of(c(date_col, asset_col, return_col))
    )

  tbl_wide <- tbl_filtered |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(asset_col),
      values_from = dplyr::all_of(return_col)
    ) |>
    dplyr::arrange(.data[[date_col]])

  matrix_returns <- tbl_wide |>
    dplyr::select(dplyr::all_of(assets)) |>
    as.matrix()

  storage.mode(matrix_returns) <- "double"

  complete_rows <- stats::complete.cases(matrix_returns)
  matrix_returns <- matrix_returns[complete_rows, , drop = FALSE]

  if (nrow(matrix_returns) < 2L) {
    rlang::abort("At least two complete return observations are required.")
  }

  list(
    mean_returns = colMeans(matrix_returns),
    covariance_matrix = stats::cov(matrix_returns),
    matrix_returns = matrix_returns,
    observations = nrow(matrix_returns)
  )
}


portfolio_bounds_GQH <- function(portfolio) {

  n_assets <- nrow(portfolio$assets)
  lower <- rep(-Inf, n_assets)
  upper <- rep(Inf, n_assets)

  box_constraints <- portfolio$constraints |>
    dplyr::filter(
      .data$type == "box",
      .data$enabled
    )

  if (nrow(box_constraints) > 0L) {
    params <- box_constraints$parameters[[nrow(box_constraints)]]
    lower <- rep(params$min, length.out = n_assets)
    upper <- rep(params$max, length.out = n_assets)
  }

  list(lower = lower, upper = upper)
}


portfolio_weight_sum_GQH <- function(portfolio) {

  constraints <- portfolio$constraints |>
    dplyr::filter(
      .data$type == "weight_sum",
      .data$enabled
    )

  if (nrow(constraints) == 0L) {
    return(list(min_sum = 1, max_sum = 1))
  }

  params <- constraints$parameters[[nrow(constraints)]]

  list(
    min_sum = params$min_sum,
    max_sum = params$max_sum
  )
}


portfolio_assert_core_constraints_GQH <- function(portfolio) {

  supported <- c("weight_sum", "box")

  unsupported <- portfolio$constraints |>
    dplyr::filter(
      .data$enabled,
      !.data$type %in% supported
    )

  if (nrow(unsupported) > 0L) {
    rlang::abort(
      paste0(
        "The current optimizer backend does not yet implement constraint type(s): ",
        paste(unique(unsupported$type), collapse = ", "),
        "."
      )
    )
  }

  weight_sum <- portfolio_weight_sum_GQH(portfolio)

  if (!isTRUE(all.equal(weight_sum$min_sum, 1)) ||
      !isTRUE(all.equal(weight_sum$max_sum, 1))) {
    rlang::abort(
      "The current optimizer backend requires `weight_sum` with min_sum = max_sum = 1."
    )
  }

  invisible(TRUE)
}


portfolio_initial_weights_GQH <- function(
    portfolio,
    lower,
    upper
) {

  weights <- as.numeric(portfolio$assets$weight)

  if (length(weights) != length(lower) ||
      any(!is.finite(weights))) {
    weights <- rep(1 / length(lower), length(lower))
  }

  portfolio_project_weights_GQH(
    weights = weights,
    lower = lower,
    upper = upper
  )
}


portfolio_project_weights_GQH <- function(
    weights,
    lower,
    upper,
    target_sum = 1,
    tolerance = 1e-12,
    max_iterations = 200L
) {

  if (any(!is.finite(lower)) || any(!is.finite(upper))) {
    rlang::abort(
      "Nonlinear portfolio optimization currently requires finite box bounds."
    )
  }

  if (sum(lower) > target_sum + tolerance ||
      sum(upper) < target_sum - tolerance) {
    rlang::abort("Box bounds are infeasible for the required weight sum.")
  }

  weights <- pmin(pmax(as.numeric(weights), lower), upper)

  for (iteration in seq_len(max_iterations)) {

    difference <- target_sum - sum(weights)

    if (abs(difference) <= tolerance) {
      return(weights)
    }

    if (difference > 0) {
      capacity <- upper - weights
      active <- capacity > tolerance
      if (!any(active)) break
      allocation <- capacity[active] / sum(capacity[active])
      weights[active] <- weights[active] +
        pmin(capacity[active], difference * allocation)
    } else {
      capacity <- weights - lower
      active <- capacity > tolerance
      if (!any(active)) break
      allocation <- capacity[active] / sum(capacity[active])
      weights[active] <- weights[active] -
        pmin(capacity[active], (-difference) * allocation)
    }
  }

  if (abs(sum(weights) - target_sum) > 1e-8) {
    rlang::abort("Unable to construct feasible portfolio weights.")
  }

  weights
}


portfolio_statistics_GQH <- function(
    weights,
    mean_returns,
    covariance_matrix
) {

  expected_return <- sum(weights * mean_returns)

  variance <- as.numeric(
    t(weights) %*%
      covariance_matrix %*%
      weights
  )

  volatility <- sqrt(max(variance, 0))

  list(
    expected_return = expected_return,
    volatility = volatility,
    variance = variance
  )
}


portfolio_ES_GQH <- function(
    portfolio_returns,
    confidence_level = 0.95
) {

  losses <- -as.numeric(portfolio_returns)

  var_level <- as.numeric(
    stats::quantile(
      losses,
      probs = confidence_level,
      names = FALSE,
      type = 7
    )
  )

  tail_losses <- losses[losses >= var_level]

  if (length(tail_losses) == 0L) {
    return(var_level)
  }

  mean(tail_losses)
}


portfolio_risk_contribution_GQH <- function(
    weights,
    covariance_matrix
) {

  portfolio_variance <- as.numeric(
    t(weights) %*%
      covariance_matrix %*%
      weights
  )

  if (!is.finite(portfolio_variance) ||
      portfolio_variance <= 0) {
    return(rep(NA_real_, length(weights)))
  }

  marginal <- as.numeric(
    covariance_matrix %*% weights
  )

  component_variance <- weights * marginal

  component_variance / portfolio_variance
}


portfolio_penalty_GQH <- function(
    weights,
    lower,
    upper,
    target_return = NULL,
    mean_returns = NULL,
    target_volatility = NULL,
    covariance_matrix = NULL,
    scale = 1e6
) {

  penalty <- scale * (sum(weights) - 1)^2

  penalty <- penalty +
    scale * sum(pmax(lower - weights, 0)^2) +
    scale * sum(pmax(weights - upper, 0)^2)

  if (!is.null(target_return)) {
    portfolio_return <- sum(weights * mean_returns)
    penalty <- penalty +
      scale * pmax(target_return - portfolio_return, 0)^2
  }

  if (!is.null(target_volatility)) {
    variance <- as.numeric(
      t(weights) %*%
        covariance_matrix %*%
        weights
    )
    volatility <- sqrt(max(variance, 0))
    penalty <- penalty +
      scale * pmax(volatility - target_volatility, 0)^2
  }

  penalty
}
