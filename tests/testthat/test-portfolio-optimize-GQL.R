test_that("ROI minimum variance optimization works", {

  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  set.seed(1234)

  n_obs <- 500L

  tbl_returns <- tibble::tibble(
    date = rep(
      seq.Date(
        as.Date("2025-01-01"),
        by = "day",
        length.out = n_obs
      ),
      times = 4L
    ),
    asset = rep(
      c(
        "Asset_A",
        "Asset_B",
        "Asset_C",
        "Asset_D"
      ),
      each = n_obs
    ),
    return = c(
      stats::rnorm(n_obs, 0.0004, 0.010),
      stats::rnorm(n_obs, 0.0003, 0.008),
      stats::rnorm(n_obs, 0.0005, 0.012),
      stats::rnorm(n_obs, 0.0002, 0.006)
    )
  )

  portfolio <- portfolio_spec_GQL(
    assets = c(
      "Asset_A",
      "Asset_B",
      "Asset_C",
      "Asset_D"
    )
  ) |>
    portfolio_constraint_GQL(
      type = "weight_sum",
      min_sum = 1,
      max_sum = 1
    ) |>
    portfolio_constraint_GQL(
      type = "box",
      min = 0,
      max = 0.60
    )

  result <- portfolio_optimize_GQL(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    method = "minimum_variance",
    solver = "ROI"
  )

  expect_s3_class(
    result,
    "portfolio_optimization_GQL"
  )

  expect_s3_class(
    result$weights,
    "tbl_df"
  )

  expect_s3_class(
    result$statistics,
    "tbl_df"
  )

  expect_equal(
    sum(result$weights$weight),
    1,
    tolerance = 1e-8
  )

  expect_true(
    all(result$weights$weight >= -1e-8)
  )

  expect_true(
    all(result$weights$weight <= 0.60 + 1e-8)
  )

  expect_equal(
    result$solver,
    "ROI::quadprog"
  )

  expect_equal(
    result$status$code,
    0
  )

  tbl_wide <- tbl_returns |>
    tidyr::pivot_wider(
      names_from = asset,
      values_from = return
    )

  matrix_returns <- tbl_wide |>
    dplyr::select(
      Asset_A,
      Asset_B,
      Asset_C,
      Asset_D
    ) |>
    as.matrix()

  covariance_matrix <- stats::cov(
    matrix_returns
  )

  equal_weights <- rep(
    0.25,
    4L
  )

  optimized_weights <- result$weights$weight

  equal_variance <- as.numeric(
    t(equal_weights) %*%
      covariance_matrix %*%
      equal_weights
  )

  optimized_variance <- as.numeric(
    t(optimized_weights) %*%
      covariance_matrix %*%
      optimized_weights
  )

  expect_lte(
    optimized_variance,
    equal_variance + 1e-12
  )

  reported_variance <- result$statistics |>
    dplyr::filter(
      .data$metric == "variance"
    ) |>
    dplyr::pull(
      .data$value
    )

  expect_equal(
    reported_variance,
    optimized_variance,
    tolerance = 1e-12
  )
})
test_that("ROI mean variance responds to risk aversion", {
  
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  
  set.seed(1234)
  
  n_obs <- 500L
  
  tbl_returns <- tibble::tibble(
    date = rep(
      seq.Date(
        as.Date("2025-01-01"),
        by = "day",
        length.out = n_obs
      ),
      times = 4L
    ),
    asset = rep(
      c(
        "Asset_A",
        "Asset_B",
        "Asset_C",
        "Asset_D"
      ),
      each = n_obs
    ),
    return = c(
      stats::rnorm(n_obs, 0.0004, 0.010),
      stats::rnorm(n_obs, 0.0003, 0.008),
      stats::rnorm(n_obs, 0.0005, 0.012),
      stats::rnorm(n_obs, 0.0002, 0.006)
    )
  )
  
  portfolio <- portfolio_spec_GQL(
    assets = c(
      "Asset_A",
      "Asset_B",
      "Asset_C",
      "Asset_D"
    )
  ) |>
    portfolio_constraint_GQL(
      type = "weight_sum",
      min_sum = 1,
      max_sum = 1
    ) |>
    portfolio_constraint_GQL(
      type = "box",
      min = 0,
      max = 0.60
    )
  
  result_low <- portfolio_optimize_GQL(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    method = "mean_variance",
    solver = "ROI",
    risk_aversion = 1
  )
  
  result_high <- portfolio_optimize_GQL(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    method = "mean_variance",
    solver = "ROI",
    risk_aversion = 100
  )
  
  vol_low <- result_low$statistics |>
    dplyr::filter(
      .data$metric == "volatility"
    ) |>
    dplyr::pull(
      .data$value
    )
  
  vol_high <- result_high$statistics |>
    dplyr::filter(
      .data$metric == "volatility"
    ) |>
    dplyr::pull(
      .data$value
    )
  
  return_low <- result_low$statistics |>
    dplyr::filter(
      .data$metric == "expected_return"
    ) |>
    dplyr::pull(
      .data$value
    )
  
  return_high <- result_high$statistics |>
    dplyr::filter(
      .data$metric == "expected_return"
    ) |>
    dplyr::pull(
      .data$value
    )
  
  expect_equal(
    sum(result_low$weights$weight),
    1,
    tolerance = 1e-8
  )
  
  expect_equal(
    sum(result_high$weights$weight),
    1,
    tolerance = 1e-8
  )
  
  expect_true(
    all(result_low$weights$weight >= -1e-8)
  )
  
  expect_true(
    all(result_high$weights$weight >= -1e-8)
  )
  
  expect_true(
    all(result_low$weights$weight <= 0.60 + 1e-8)
  )
  
  expect_true(
    all(result_high$weights$weight <= 0.60 + 1e-8)
  )
  
  expect_lt(
    vol_high,
    vol_low
  )
  
  expect_gt(
    return_low,
    return_high
  )
  
  expect_equal(
    result_low$status$code,
    0
  )
  
  expect_equal(
    result_high$status$code,
    0
  )
})

test_that("ROI maximum return optimization works", {
  
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  
  set.seed(1234)
  
  n_obs <- 500L
  
  tbl_returns <- tibble::tibble(
    date = rep(
      seq.Date(
        as.Date("2025-01-01"),
        by = "day",
        length.out = n_obs
      ),
      times = 4L
    ),
    asset = rep(
      c(
        "Asset_A",
        "Asset_B",
        "Asset_C",
        "Asset_D"
      ),
      each = n_obs
    ),
    return = c(
      stats::rnorm(n_obs, 0.0004, 0.010),
      stats::rnorm(n_obs, 0.0003, 0.008),
      stats::rnorm(n_obs, 0.0005, 0.012),
      stats::rnorm(n_obs, 0.0002, 0.006)
    )
  )
  
  portfolio <- portfolio_spec_GQL(
    assets = c(
      "Asset_A",
      "Asset_B",
      "Asset_C",
      "Asset_D"
    )
  ) |>
    portfolio_constraint_GQL(
      type = "weight_sum",
      min_sum = 1,
      max_sum = 1
    ) |>
    portfolio_constraint_GQL(
      type = "box",
      min = 0,
      max = 0.60
    )
  
  result <- portfolio_optimize_GQL(
    tbl_returns = tbl_returns,
    portfolio = portfolio,
    method = "maximum_return",
    solver = "ROI"
  )
  
  expect_s3_class(
    result,
    "portfolio_optimization_GQL"
  )
  
  expect_equal(
    result$solver,
    "ROI::glpk"
  )
  
  expect_equal(
    result$status$code,
    0
  )
  
  expect_equal(
    sum(result$weights$weight),
    1,
    tolerance = 1e-8
  )
  
  expect_true(
    all(result$weights$weight >= -1e-8)
  )
  
  expect_true(
    all(result$weights$weight <= 0.60 + 1e-8)
  )
  
  mean_returns <- tbl_returns |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(
      mean_return = mean(.data$return),
      .groups = "drop"
    )
  
  asset_max <- mean_returns |>
    dplyr::slice_max(
      .data$mean_return,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::pull(.data$asset)
  
  weight_max <- result$weights |>
    dplyr::filter(
      .data$asset == asset_max
    ) |>
    dplyr::pull(.data$weight)
  
  expect_equal(
    weight_max,
    0.60,
    tolerance = 1e-8
  )
})