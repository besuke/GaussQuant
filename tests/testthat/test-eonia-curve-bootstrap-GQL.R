test_that("Ametrano-Bianchetti EONIA quotes match the Cookbook inputs", {
  quotes <- ametrano_bianchetti_eonia_quotes_GQL()

  expect_equal(nrow(quotes), 30L)
  expect_equal(sum(quotes$instrument_type == "deposit"), 3L)
  expect_equal(sum(quotes$instrument_type == "dated_ois"), 5L)
  expect_equal(sum(quotes$instrument_type == "spot_ois"), 22L)
  expect_equal(quotes$rate, quotes$rate_pct / 100, tolerance = 0)

  expected_long_rates <- c(
    0.002, 0.008, 0.021, 0.036, 0.127, 0.274, 0.456, 0.647,
    0.827, 0.996, 1.147, 1.280, 1.404, 1.516, 1.764, 1.939,
    2.003, 2.038
  ) / 100

  expect_equal(
    quotes$rate[quotes$quote_id %in% c(
      "OIS-15M", "OIS-18M", "OIS-21M", "OIS-2Y", "OIS-3Y",
      "OIS-4Y", "OIS-5Y", "OIS-6Y", "OIS-7Y", "OIS-8Y",
      "OIS-9Y", "OIS-10Y", "OIS-11Y", "OIS-12Y", "OIS-15Y",
      "OIS-20Y", "OIS-25Y", "OIS-30Y"
    )],
    expected_long_rates,
    tolerance = 0
  )
})


test_that("EONIA curve reprices all market helpers", {
  skip_if_not_installed("QuantLib")

  bundle <- build_eonia_curve_from_market_GQL()
  validation <- eonia_curve_validation_GQL(bundle)
  errors <- validation$quote_repricing$quote_error

  expect_false(anyNA(errors))
  expect_lt(max(abs(errors)), 1e-10)
})


test_that("EONIA flat-forward nodes reproduce the Cookbook benchmark", {
  skip_if_not_installed("QuantLib")

  benchmark <- eonia_curve_benchmark_GQL()
  expected <- tibble::tribble(
    ~node_date, ~node_rate,
    as.Date("2012-12-11"), 0.00040555533025081675,
    as.Date("2012-12-12"), 0.00040555533025081675,
    as.Date("2012-12-13"), 0.00040555533047721286,
    as.Date("2012-12-14"), 0.00040555533047721286,
    as.Date("2012-12-20"), 0.0007604110692568178,
    as.Date("2012-12-27"), 0.0006894305026004767,
    as.Date("2013-01-03"), 0.0009732981324671213,
    as.Date("2013-01-14"), 0.0006728161005748453,
    as.Date("2013-02-13"), 0.00046638054590758754
  )

  actual <- benchmark$flat_nodes |>
    dplyr::filter(.data$node_date %in% expected$node_date) |>
    dplyr::arrange(.data$node_date)

  expect_equal(actual$node_date, expected$node_date)
  expect_equal(actual$node_rate, expected$node_rate, tolerance = 1e-9)
})


test_that("EONIA year-end jump matches the rounded Cookbook results", {
  skip_if_not_installed("QuantLib")

  jump <- eonia_curve_benchmark_GQL()$jump_summary

  expect_equal(100 * jump$original_forward, 0.082, tolerance = 0.001)
  expect_equal(100 * jump$clean_forward, 0.067, tolerance = 0.001)
  expect_equal(100 * jump$jump_rate, 0.101, tolerance = 0.001)
})
