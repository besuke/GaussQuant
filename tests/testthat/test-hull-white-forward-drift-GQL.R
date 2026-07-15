test_that("Hull-White forward drift matches QuantLib M_T", {
  start_time <- 0.25
  exercise_time <- 1.50
  maturity <- 4.00
  mean_reversion <- 0.03
  sigma <- 0.01

  coefficient <- sigma^2 / mean_reversion^2
  expected <- coefficient * (
    1 - exp(
      -mean_reversion *
        (exercise_time - start_time)
    )
  ) - 0.5 * coefficient * (
    exp(
      -mean_reversion *
        (maturity - exercise_time)
    ) -
      exp(
        -mean_reversion *
          (maturity + exercise_time - 2 * start_time)
      )
  )

  actual <- hull_white_forward_drift_GQL(
    start_time = start_time,
    exercise_time = exercise_time,
    maturity = maturity,
    mean_reversion = mean_reversion,
    sigma = sigma
  )

  expect_equal(
    actual,
    expected,
    tolerance = 1e-14
  )
})

test_that("Hull-White forward drift uses the low-a algebraic limit", {
  start_time <- 0.25
  exercise_time <- 1.50
  maturity <- 4.00
  sigma <- 0.01

  expected <- sigma^2 / 2 *
    (exercise_time - start_time) *
    (2 * maturity - exercise_time - start_time)

  actual <- hull_white_forward_drift_GQL(
    start_time = start_time,
    exercise_time = exercise_time,
    maturity = maturity,
    mean_reversion = 0,
    sigma = sigma
  )

  expect_equal(
    actual,
    expected,
    tolerance = 1e-14
  )
})

test_that("Hull-White forward drift has the correct one-year special case", {
  mean_reversion <- 0.03
  sigma <- 0.01
  exercise_time <- 1

  expected <- sigma^2 / (2 * mean_reversion^2) *
    (1 - exp(-mean_reversion * exercise_time))^2

  actual <- hull_white_forward_drift_GQL(
    start_time = 0,
    exercise_time = exercise_time,
    maturity = exercise_time,
    mean_reversion = mean_reversion,
    sigma = sigma
  )

  expect_equal(
    actual,
    expected,
    tolerance = 1e-14
  )
})
