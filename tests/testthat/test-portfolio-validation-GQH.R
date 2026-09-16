test_that("box constraint validates weight bounds", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "box",
        min = 0.50,
        max = 0.20
      ),
    "Each `min` weight"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "box",
        min = 0,
        max = 0.40
      )
  )
})


test_that("box constraint accepts asset-specific bounds", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  result <- portfolio |>
    portfolio_constraint_GQL(
      type = "box",
      min = c(0, 0.10, 0.20),
      max = c(0.50, 0.60, 0.70)
    )
  
  expect_equal(
    result$constraints$parameters[[1]]$min,
    c(0, 0.10, 0.20)
  )
})


test_that("weight_sum constraint validates bounds", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "weight_sum",
        min_sum = 1.10,
        max_sum = 1.00
      ),
    "`min_sum` must be less than or equal to `max_sum`"
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "weight_sum",
        min_sum = 1
      ),
    "`weight_sum` requires"
  )
})


test_that("group constraint validates asset mapping", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "group",
        groups = c("Equity", "Bond"),
        group_min = 0,
        group_max = 1
      ),
    "`groups` must have one value for each asset"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "group",
        groups = c("Equity", "Equity", "Bond"),
        group_min = c(0, 0),
        group_max = c(1, 1)
      )
  )
})


test_that("factor exposure validates number of assets", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  factor_loadings <- matrix(
    c(
      1.0, 0.5,
      0.8, 0.3
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "factor_exposure",
        factor_loadings = factor_loadings,
        min = -1,
        max = 1
      ),
    "one row per asset"
  )
})


test_that("turnover constraint validates target", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "turnover",
        target = -0.10
      ),
    "non-negative"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "turnover",
        target = 0.20
      )
  )
})


test_that("leverage constraint validates maximum leverage", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "leverage",
        max_leverage = 0
      ),
    "positive numeric scalar"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_constraint_GQL(
        type = "leverage",
        max_leverage = 1.50
      )
  )
})


test_that("risk objective validates supported measures", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "ABC"
      ),
    "Unsupported measure"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "StdDev"
      )
  )
  
  expect_no_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "ES"
      )
  )
})


test_that("ES objective validates confidence level", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "ES",
        confidence_level = 1.20
      ),
    "`confidence_level` must be between 0 and 1"
  )
  
  expect_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "ES",
        confidence_level = 0
      ),
    "`confidence_level` must be between 0 and 1"
  )
  
  expect_no_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "ES",
        confidence_level = 0.975
      )
  )
})


test_that("VaR objective validates confidence level", {
  
  portfolio <- portfolio_spec_GQL(
    c("A", "B", "C")
  )
  
  expect_no_error(
    portfolio |>
      portfolio_objective_GQL(
        type = "risk",
        measure = "VaR",
        confidence_level = 0.99
      )
  )
})