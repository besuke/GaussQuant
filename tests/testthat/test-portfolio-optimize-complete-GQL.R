test_that("portfolio optimizer supports the complete method set", {

  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
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
      c("Asset_A", "Asset_B", "Asset_C", "Asset_D"),
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
    assets = c("Asset_A", "Asset_B", "Asset_C", "Asset_D")
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

  result_min_var <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "minimum_variance",
    solver = "ROI"
  )

  result_mean_var <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "mean_variance",
    solver = "ROI",
    risk_aversion = 100
  )

  result_max_return <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "maximum_return",
    solver = "ROI"
  )

  result_max_sharpe <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "maximum_sharpe",
    solver = "ROI",
    risk_free_rate = 0
  )

  result_min_es <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "minimum_ES",
    solver = "ROI",
    confidence_level = 0.95
  )

  result_risk_budget <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "risk_budget",
    solver = "ROI"
  )

  target_return <- result_min_var$statistics |>
    dplyr::filter(.data$metric == "expected_return") |>
    dplyr::pull(.data$value)

  result_target_return <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "target_return",
    solver = "ROI",
    target_return = target_return
  )

  max_return_vol <- result_max_return$statistics |>
    dplyr::filter(.data$metric == "volatility") |>
    dplyr::pull(.data$value)

  result_target_risk <- portfolio_optimize_GQL(
    tbl_returns,
    portfolio,
    method = "target_risk",
    solver = "ROI",
    target_volatility = max_return_vol
  )

  results <- list(
    result_min_var,
    result_mean_var,
    result_max_return,
    result_max_sharpe,
    result_min_es,
    result_risk_budget,
    result_target_return,
    result_target_risk
  )

  purrr::walk(
    results,
    function(result) {
      expect_s3_class(result, "portfolio_optimization_GQL")
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
    }
  )

  expect_equal(result_min_var$solver, "ROI::quadprog")
  expect_equal(result_mean_var$solver, "ROI::quadprog")
  expect_equal(result_max_return$solver, "ROI::glpk")
  expect_equal(result_target_return$solver, "ROI::quadprog")

  expect_equal(result_max_sharpe$status$code, 0)
  expect_equal(result_min_es$status$code, 0)
  expect_equal(result_risk_budget$status$code, 0)
  expect_equal(result_target_risk$status$code, 0)

  risk_contribution <- portfolio_risk_contribution_GQH(
    result_risk_budget$weights$weight,
    stats::cov(
      tbl_returns |>
        tidyr::pivot_wider(
          names_from = asset,
          values_from = return
        ) |>
        dplyr::select(
          Asset_A,
          Asset_B,
          Asset_C,
          Asset_D
        ) |>
        as.matrix()
    )
  )

  expect_equal(
    sum(risk_contribution),
    1,
    tolerance = 1e-8
  )
})
