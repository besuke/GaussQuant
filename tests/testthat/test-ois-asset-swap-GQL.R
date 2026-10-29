# Regression tests for the OIS curve options and OIS asset swaps migrated from
# LagrangeFinance R/. The fixture holds synthetic quotes and the outputs of the
# original LagrangeFinance implementation (an independent code path) computed
# before the migration; no market data is stored.

baseline_GQL <- function() {
  readRDS(test_path("fixtures", "ois_asset_swap_lf_baseline.rds"))
}

baseline_marketdate_GQL <- "2026-10-06"

build_baseline_curve_GQL <- function(quotes, currency, curve_type = "log_cubic_discount") {
  ir_build_ois_curve_GQH(
    curve_data = quotes[quotes$currency == currency, ] |>
      dplyr::transmute(kind = "ois", tenor, rate = rate_pct / 100),
    trade_date = baseline_marketdate_GQL,
    currency = currency,
    verbose = FALSE,
    curve_type = curve_type,
    reference = "evaluation",
    payment_frequency = "Annual",
    payment_lag = 0L,
    payment_convention = "ModifiedFollowing",
    max_quote_error_bp = 1e-4
  )
}

build_baseline_bond_GQL <- function(currency, coupon_rate, calendar) {
  fixed_rate_bond_GQL(
    issue_date = "2026-09-30",
    maturity_date = "2036-09-30",
    coupon_rate = coupon_rate,
    face_amount = 100,
    redemption = 100,
    settlement_days = 2L,
    frequency = QuantLib::Period("Semiannual"),
    calendar = calendar,
    accrual_day_counter = if (currency == "JPY") {
      QuantLib::Actual365Fixed()
    } else {
      QuantLib::Thirty360("BondBasis")
    },
    payment_convention = "Following",
    schedule_convention = "Unadjusted",
    maturity_convention = "Unadjusted"
  )
}

baseline_bond_tbl_GQL <- tibble::tribble(
  ~currency, ~coupon_rate, ~fx_spot, ~basis_bp,
  "EUR", 0.035, 165, -20,
  "USD", 0.050, 150, -30,
  "JPY", 0.030, 1, 0,
  "GBP", 0.050, 195, -25
)

build_baseline_set_GQL <- function() {
  fx <- baseline_GQL()
  currencies <- baseline_bond_tbl_GQL$currency
  curves <- purrr::set_names(currencies) |>
    purrr::map(function(ccy) build_baseline_curve_GQL(fx$quotes, ccy))
  bonds <- purrr::set_names(currencies) |>
    purrr::map(function(ccy) {
      row <- baseline_bond_tbl_GQL[baseline_bond_tbl_GQL$currency == ccy, ]
      build_baseline_bond_GQL(ccy, row$coupon_rate, curves[[ccy]]$calendar)
    })
  list(fixture = fx, curves = curves, bonds = bonds)
}

test_that("OIS convention table covers ESTR, SOFR, TONA and SONIA", {
  skip_if_not_installed("QuantLib")

  conv <- ir_ois_convention_table_GQH()

  expect_equal(conv$currency, c("EUR", "USD", "JPY", "GBP"))
  expect_equal(conv$instrument, c("ESTR", "SOFR", "TONA", "SONIA"))
  expect_equal(conv$settlement_days, c(2L, 2L, 2L, 0L))
  expect_equal(conv$day_counter, c("Actual/360", "Actual/360", "Actual/365 (Fixed)", "Actual/365 (Fixed)"))
})

test_that("OIS curves reproduce the original LagrangeFinance curves", {
  skip_if_not_installed("QuantLib")

  fx <- baseline_GQL()

  for (type in c("log_cubic_discount", "flat_forward")) {
    for (ccy in unique(fx$discount$currency)) {
      curve <- build_baseline_curve_GQL(fx$quotes, ccy, curve_type = type)
      expected <- fx$discount[fx$discount$curve_type == type & fx$discount$currency == ccy, ]
      set_eval_date_GQL(baseline_marketdate_GQL)
      actual <- purrr::map_dbl(
        as.character(expected$date),
        function(d) curve$curve$discount(date_GQL(d))
      )

      expect_equal(actual, expected$discount, tolerance = 1e-12, info = paste(type, ccy))
      expect_lt(max(abs(curve$quote_repricing$quote_error_bp)), 1e-4)
    }
  }
})

test_that("Default OIS curve matches an explicit flat-forward build", {
  skip_if_not_installed("QuantLib")

  fx <- baseline_GQL()
  quotes <- fx$quotes[fx$quotes$currency == "USD", ] |>
    dplyr::transmute(kind = "ois", tenor, rate = rate_pct / 100)

  default_curve <- ir_build_ois_curve_GQH(quotes, baseline_marketdate_GQL, "USD", verbose = FALSE)
  flat_curve <- ir_build_ois_curve_GQH(
    quotes, baseline_marketdate_GQL, "USD",
    verbose = FALSE, curve_type = "flat_forward", reference = "evaluation"
  )

  expect_equal(default_curve$curve_type, "log_linear_discount")
  expect_equal(default_curve$reference, "settlement")

  settle <- default_curve$curve$referenceDate()
  dates <- as.character(default_curve$quote_repricing$maturity_date)
  ratio <- function(curve) {
    purrr::map_dbl(dates, function(d) curve$discount(date_GQL(d)) / curve$discount(settle))
  }

  expect_equal(ratio(flat_curve$curve), ratio(default_curve$curve), tolerance = 1e-12)
})

test_that("OIS curve rejects duplicate tenors", {
  skip_if_not_installed("QuantLib")

  quotes <- tibble::tibble(kind = "ois", tenor = c("1Y", "1Y"), rate = c(0.01, 0.011))

  expect_error(
    ir_build_ois_curve_GQH(quotes, baseline_marketdate_GQL, "EUR", verbose = FALSE),
    "duplicate tenors"
  )
})

test_that("OIS asset swaps reproduce the original LagrangeFinance results", {
  skip_if_not_installed("QuantLib")

  set <- build_baseline_set_GQL()
  fx <- set$fixture

  for (ccy in names(set$curves)) {
    asw <- ois_asset_swap_GQL(set$bonds[[ccy]], 99.5, set$curves[[ccy]])
    expected <- fx$native[fx$native$currency == ccy, ]

    for (col in c("dirty_price", "fair_spread_bp", "spread_annuity", "npv",
                  "bond_leg_pv", "overnight_leg_pv", "principal_pv", "upfront_pv")) {
      expect_equal(asw$valuation[[col]], expected[[col]], tolerance = 1e-10, info = paste(ccy, col))
    }
    expect_equal(ois_asset_swap_fair_price_GQL(asw), expected$fair_price, tolerance = 1e-10)
    expect_equal(
      ois_asset_swap_price_table_GQL(asw, c(98, 99.5, 101))$AS_spread_bp,
      fx$price_table$AS_spread_bp[fx$price_table$currency == ccy],
      tolerance = 1e-10
    )

    repriced <- ois_asset_swap_GQL(
      set$bonds[[ccy]], 99.5, set$curves[[ccy]],
      spread_bp = asw$valuation$fair_spread_bp
    )
    expect_lt(abs(repriced$valuation$npv), 1e-9)
    expect_equal(ois_asset_swap_fair_price_GQL(repriced), 99.5, tolerance = 1e-10)
  }
})

test_that("JPY basis asset swaps reproduce the original LagrangeFinance results", {
  skip_if_not_installed("QuantLib")

  set <- build_baseline_set_GQL()
  fx <- set$fixture

  for (ccy in c("EUR", "USD", "GBP")) {
    row <- baseline_bond_tbl_GQL[baseline_bond_tbl_GQL$currency == ccy, ]
    jpy <- ois_asset_swap_jpy_basis_GQL(
      bond = set$bonds[[ccy]],
      clean_price = 99.5,
      foreign_curve = set$curves[[ccy]],
      jpy_curve = set$curves$JPY,
      fx_spot = row$fx_spot,
      basis_bp = row$basis_bp,
      currency = ccy
    )
    expected <- fx$jpy[fx$jpy$currency == ccy, ]

    for (col in c("native_asw_bp", "jpy_asw_bp", "simple_sum_bp", "calibrated_discount_spread_bp",
                  "hedged_bond_pv_jpy", "spread_annuity_jpy")) {
      expect_equal(jpy$valuation[[col]], expected[[col]], tolerance = 1e-8, info = paste(ccy, col))
    }
    expect_equal(
      jpy$hedged_bond_cashflows$fx_forward,
      fx$fx_forward$fx_forward[fx$fx_forward$currency == ccy],
      tolerance = 1e-10
    )
    expect_lt(abs(jpy$valuation$ccs_repricing_npv_jpy), 1e-6)
    expect_lt(abs(jpy$valuation$fair_spread_npv_jpy), 1e-6)
  }
})

test_that("AONIA and overnight deposits build and reprice", {
  skip_if_not_installed("QuantLib")

  conv <- ir_ois_convention_table_GQH("AUD")
  expect_equal(conv$instrument, "AONIA")
  expect_equal(conv$settlement_days, 1L)

  quotes <- tibble::tibble(
    kind = c("overnight_deposit", "ois", "ois", "ois"),
    tenor = c("1D", "3M", "6M", "1Y"),
    rate = c(0.0410, 0.0412, 0.0415, 0.0420)
  )
  curve <- ir_build_ois_curve_GQH(
    quotes, baseline_marketdate_GQL, "AUD",
    verbose = FALSE, reference = "evaluation", max_quote_error_bp = 1e-6
  )

  expect_equal(curve$instrument, "AONIA")
  expect_lt(max(abs(curve$quote_repricing$quote_error_bp)), 1e-6)
  expect_error(
    ir_build_ois_curve_GQH(
      tibble::tibble(kind = "overnight_deposit", tenor = "1W", rate = 0.04),
      baseline_marketdate_GQL, "AUD", verbose = FALSE
    ),
    "only the 1D tenor"
  )
})

test_that("A trade-date 1D deposit is kept only with an evaluation-date reference", {
  skip_if_not_installed("QuantLib")

  quotes <- tibble::tibble(
    as_of_date = as.Date("2023-09-26"), currency = "USD", instrument = "SOFR",
    kind = c("depo", rep("ois", 6)),
    tenor = c("1D", "1M", "3M", "6M", "1Y", "2Y", "3Y"),
    rate = c(0.0531, 0.0532, 0.0538, 0.0546, 0.0545, 0.0501, 0.0467)
  )

  settlement <- ir_build_ois_curve_envs_GQH(quotes, verbose = FALSE)[["USD::SOFR"]]
  evaluation <- ir_build_ois_curve_envs_GQH(
    quotes, verbose = FALSE, reference = "evaluation", max_quote_error_bp = 1e-6
  )[["USD::SOFR"]]

  expect_false(settlement$quote_repricing$used_in_bootstrap[1])
  expect_true(all(evaluation$quote_repricing$used_in_bootstrap))
  expect_lt(max(abs(evaluation$quote_repricing$quote_error_bp)), 1e-6)
  expect_equal(evaluation$reference, "evaluation")
})

test_that("TONA O/N deposit starts on the trade date while OIS start at spot", {
  skip_if_not_installed("QuantLib")

  quotes <- tibble::tibble(
    kind = c("overnight_deposit", "ois", "ois"),
    tenor = c("1D", "1W", "1M"),
    rate = c(-0.0000900, -0.0001261, -0.0001807)
  )
  curve <- ir_build_ois_curve_GQH(
    quotes, "2022-08-19", "JPY",
    verbose = FALSE, reference = "evaluation", max_quote_error_bp = 1e-6
  )
  rp <- curve$quote_repricing

  expect_equal(as.character(rp$start_date), c("2022-08-19", "2022-08-23", "2022-08-23"))
  expect_equal(as.character(rp$maturity_date[1]), "2022-08-22")
  expect_true(all(rp$used_in_bootstrap))

  spot_deposit <- ir_build_ois_curve_GQH(
    dplyr::mutate(quotes, kind = c("depo", "ois", "ois")), "2022-08-19", "JPY", verbose = FALSE
  )$quote_repricing
  expect_equal(as.character(spot_deposit$start_date[1]), "2022-08-23")
})

test_that("Evaluation-date reference date stays fixed when the evaluation date moves", {
  skip_if_not_installed("QuantLib")

  quotes <- tibble::tibble(kind = "ois", tenor = c("1W", "1M", "1Y"), rate = c(0.001, 0.0012, 0.0015))
  curve <- ir_build_ois_curve_GQH(quotes, "2022-08-19", "JPY", verbose = FALSE, reference = "evaluation")

  set_eval_date_GQL("2022-08-25")
  on.exit(set_eval_date_GQL("2022-08-19"))

  expect_equal(QuantLib::Date_ISO(curve$curve$referenceDate()), "2022-08-19")
})
