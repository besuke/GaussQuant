test_that("portfolio_spec_GQL creates a valid portfolio specification", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C", "Asset_D")
  )
  
  expect_s3_class(
    portfolio,
    "portfolio_spec_GQL"
  )
  
  expect_true(
    all(c("assets", "constraints", "objectives") %in% names(portfolio))
  )
  
  expect_s3_class(
    portfolio$assets,
    "tbl_df"
  )
  
  expect_equal(
    portfolio$assets$asset,
    c("Asset_A", "Asset_B", "Asset_C", "Asset_D")
  )
  
  expect_equal(
    portfolio$assets$weight,
    rep(0.25, 4)
  )
  
  expect_equal(
    sum(portfolio$assets$weight),
    1
  )
})


test_that("portfolio_spec_GQL accepts user supplied weights", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C"),
    weights = c(0.50, 0.30, 0.20)
  )
  
  expect_equal(
    portfolio$assets$weight,
    c(0.50, 0.30, 0.20)
  )
})


test_that("portfolio_spec_GQL rejects invalid asset specifications", {
  
  expect_error(
    portfolio_spec_GQL(
      assets = character()
    ),
    "`assets` must contain at least one asset"
  )
  
  expect_error(
    portfolio_spec_GQL(
      assets = c("Asset_A", "Asset_A")
    ),
    "`assets` must contain unique asset names"
  )
  
  expect_error(
    portfolio_spec_GQL(
      assets = c("Asset_A", "Asset_B"),
      weights = c(1)
    ),
    "`weights` must have the same length as `assets`"
  )
})


test_that("portfolio_constraint_GQL adds weight sum constraint", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C")
  ) |>
    portfolio_constraint_GQL(
      type = "weight_sum",
      min_sum = 1,
      max_sum = 1
    )
  
  expect_equal(
    nrow(portfolio$constraints),
    1
  )
  
  expect_equal(
    portfolio$constraints$type,
    "weight_sum"
  )
  
  expect_true(
    portfolio$constraints$enabled
  )
  
  expect_equal(
    portfolio$constraints$parameters[[1]]$min_sum,
    1
  )
  
  expect_equal(
    portfolio$constraints$parameters[[1]]$max_sum,
    1
  )
})


test_that("portfolio_constraint_GQL supports multiple constraints", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C")
  ) |>
    portfolio_constraint_GQL(
      type = "weight_sum",
      min_sum = 1,
      max_sum = 1
    ) |>
    portfolio_constraint_GQL(
      type = "box",
      min = 0,
      max = 0.40
    )
  
  expect_equal(
    nrow(portfolio$constraints),
    2
  )
  
  expect_equal(
    portfolio$constraints$constraint_id,
    c(1L, 2L)
  )
  
  expect_equal(
    portfolio$constraints$type,
    c("weight_sum", "box")
  )
  
  expect_equal(
    portfolio$constraints$parameters[[2]]$min,
    0
  )
  
  expect_equal(
    portfolio$constraints$parameters[[2]]$max,
    0.40
  )
})


test_that("portfolio_constraint_GQL rejects invalid input", {
  
  expect_error(
    portfolio_constraint_GQL(
      list(),
      type = "box"
    ),
    "`portfolio` must be created by"
  )
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B")
  )
  
  expect_error(
    portfolio_constraint_GQL(
      portfolio,
      type = "not_a_constraint"
    )
  )
})


test_that("portfolio_objective_GQL adds return objective", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C")
  ) |>
    portfolio_objective_GQL(
      type = "return",
      measure = "mean"
    )
  
  expect_equal(
    nrow(portfolio$objectives),
    1
  )
  
  expect_equal(
    portfolio$objectives$type,
    "return"
  )
  
  expect_equal(
    portfolio$objectives$measure,
    "mean"
  )
  
  expect_true(
    portfolio$objectives$enabled
  )
})


test_that("portfolio_objective_GQL supports multiple objectives", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B", "Asset_C")
  ) |>
    portfolio_objective_GQL(
      type = "return",
      measure = "mean"
    ) |>
    portfolio_objective_GQL(
      type = "risk",
      measure = "StdDev"
    )
  
  expect_equal(
    nrow(portfolio$objectives),
    2
  )
  
  expect_equal(
    portfolio$objectives$objective_id,
    c(1L, 2L)
  )
  
  expect_equal(
    portfolio$objectives$type,
    c("return", "risk")
  )
  
  expect_equal(
    portfolio$objectives$measure,
    c("mean", "StdDev")
  )
})


test_that("portfolio_objective_GQL stores additional parameters", {
  
  portfolio <- portfolio_spec_GQL(
    assets = c("Asset_A", "Asset_B")
  ) |>
    portfolio_objective_GQL(
      type = "risk",
      measure = "ES",
      confidence_level = 0.975
    )
  
  expect_equal(
    portfolio$objectives$parameters[[1]]$confidence_level,
    0.975
  )
})


test_that("portfolio specification works through the complete pipe", {
  
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
      max = 0.40
    ) |>
    portfolio_objective_GQL(
      type = "return",
      measure = "mean"
    ) |>
    portfolio_objective_GQL(
      type = "risk",
      measure = "StdDev"
    )
  
  expect_s3_class(
    portfolio,
    "portfolio_spec_GQL"
  )
  
  expect_equal(
    nrow(portfolio$assets),
    4
  )
  
  expect_equal(
    nrow(portfolio$constraints),
    2
  )
  
  expect_equal(
    nrow(portfolio$objectives),
    2
  )
  
  expect_equal(
    portfolio$constraints$type,
    c("weight_sum", "box")
  )
  
  expect_equal(
    portfolio$objectives$type,
    c("return", "risk")
  )
})