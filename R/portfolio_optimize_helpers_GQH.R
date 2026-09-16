validate_returns_GQH <- function(
    tbl_returns,
    portfolio,
    date_col,
    asset_col,
    return_col
) {
  
  if (!inherits(tbl_returns, "data.frame")) {
    rlang::abort(
      "`tbl_returns` must be a data frame or tibble."
    )
  }
  
  required_columns <- c(
    date_col,
    asset_col,
    return_col
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(tbl_returns)
  )
  
  if (length(missing_columns) > 0L) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      )
    )
  }
  
  assets_data <- unique(
    as.character(tbl_returns[[asset_col]])
  )
  
  missing_assets <- setdiff(
    portfolio$assets$asset,
    assets_data
  )
  
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
    rlang::abort(
      "The return column must be numeric."
    )
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
    dplyr::filter(
      .data[[asset_col]] %in% assets
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          date_col,
          asset_col,
          return_col
        )
      )
    )
  
  tbl_wide <- tbl_filtered |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(asset_col),
      values_from = dplyr::all_of(return_col)
    ) |>
    dplyr::arrange(
      .data[[date_col]]
    )
  
  matrix_returns <- tbl_wide |>
    dplyr::select(
      dplyr::all_of(assets)
    ) |>
    as.matrix()
  
  storage.mode(matrix_returns) <- "double"
  
  complete_rows <- stats::complete.cases(
    matrix_returns
  )
  
  matrix_returns <- matrix_returns[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  if (nrow(matrix_returns) < 2L) {
    rlang::abort(
      "At least two complete return observations are required."
    )
  }
  
  mean_returns <- colMeans(
    matrix_returns
  )
  
  covariance_matrix <- stats::cov(
    matrix_returns
  )
  
  list(
    mean_returns = mean_returns,
    covariance_matrix = covariance_matrix,
    observations = nrow(matrix_returns)
  )
}


portfolio_bounds_GQH <- function(portfolio) {
  
  n_assets <- nrow(portfolio$assets)
  
  lower <- rep(
    -Inf,
    n_assets
  )
  
  upper <- rep(
    Inf,
    n_assets
  )
  
  box_constraints <- portfolio$constraints |>
    dplyr::filter(
      .data$type == "box",
      .data$enabled
    )
  
  if (nrow(box_constraints) > 0L) {
    
    params <- box_constraints$parameters[[nrow(box_constraints)]]
    
    lower <- rep(
      params$min,
      length.out = n_assets
    )
    
    upper <- rep(
      params$max,
      length.out = n_assets
    )
  }
  
  list(
    lower = lower,
    upper = upper
  )
}


portfolio_initial_weights_GQH <- function(
    portfolio,
    lower,
    upper
) {
  
  weights <- portfolio$assets$weight
  
  weights <- pmax(
    weights,
    lower
  )
  
  weights <- pmin(
    weights,
    upper
  )
  
  total <- sum(weights)
  
  if (!is.finite(total) ||
      total == 0) {
    weights <- rep(
      1 / length(weights),
      length(weights)
    )
  } else {
    weights <- weights / total
  }
  
  weights
}


portfolio_objective_value_GQH <- function(
    weights,
    mean_returns,
    covariance_matrix,
    method,
    risk_aversion
) {
  
  weights <- weights / sum(weights)
  
  expected_return <- sum(
    weights * mean_returns
  )
  
  variance <- as.numeric(
    t(weights) %*%
      covariance_matrix %*%
      weights
  )
  
  switch(
    method,
    
    minimum_variance =
      variance,
    
    mean_variance =
      risk_aversion * variance -
      expected_return,
    
    maximum_return =
      -expected_return
  )
}


portfolio_normalize_weights_GQH <- function(
    weights,
    portfolio
) {
  
  weight_sum_constraints <- portfolio$constraints |>
    dplyr::filter(
      .data$type == "weight_sum",
      .data$enabled
    )
  
  if (nrow(weight_sum_constraints) == 0L) {
    return(weights)
  }
  
  params <- weight_sum_constraints$parameters[[1]]
  
  if (isTRUE(
    all.equal(
      params$min_sum,
      1
    )
  ) &&
  isTRUE(
    all.equal(
      params$max_sum,
      1
    )
  )) {
    
    total <- sum(weights)
    
    if (!is.finite(total) ||
        total == 0) {
      rlang::abort(
        "Unable to normalize portfolio weights."
      )
    }
    
    weights <- weights / total
  }
  
  weights
}


portfolio_statistics_GQH <- function(
    weights,
    mean_returns,
    covariance_matrix
) {
  
  expected_return <- sum(
    weights * mean_returns
  )
  
  variance <- as.numeric(
    t(weights) %*%
      covariance_matrix %*%
      weights
  )
  
  volatility <- sqrt(
    max(
      variance,
      0
    )
  )
  
  list(
    expected_return = expected_return,
    volatility = volatility,
    variance = variance
  )
}