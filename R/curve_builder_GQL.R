# R/curve_builder.R

#' Build a QuantLib discount curve from zero-rate nodes
#'
#' @param nodes A data frame with date and zero_rate columns.
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return A QuantLib DiscountCurve object.
#'
#' @export
build_discount_curve_GQL <- function(
    nodes,
    day_counter = "Actual365Fixed"
) {
  stopifnot(is.data.frame(nodes))
  stopifnot(all(c("date", "zero_rate") %in% names(nodes)))

  dates_chr <- as.character(nodes$date)
  dates_ql <- make_date_vector_GQL(dates_chr)

  dc <- day_counter_GQL(day_counter)

  origin <- as.Date(dates_chr[[1]])
  times <- as.numeric(as.Date(dates_chr) - origin) / 365
  times[1] <- 0

  dfs <- exp(-as.numeric(nodes$zero_rate) * times)
  dfs[1] <- 1.0

  QuantLib::DiscountCurve(
    dates_ql,
    dfs,
    dc
  )
}

#' Build a QuantLib zero curve from zero-rate nodes
#'
#' @param date_chr Character vector of ISO dates.
#' @param zero_rates Numeric vector of zero rates.
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return A QuantLib ZeroCurve object.
#'
#' @export
build_zero_curve_GQL <- function(
    date_chr,
    zero_rates,
    day_counter = "Actual365Fixed"
) {
  dates_ql <- make_date_vector_GQL(as.character(date_chr))
  dc <- day_counter_GQL(day_counter)

  QuantLib::ZeroCurve(
    dates_ql,
    as.numeric(zero_rates),
    dc
  )
}


#' Build a QuantLib yield curve handle
#'
#' @param curve A QuantLib yield term structure object.
#'
#' @return A QuantLib YieldTermStructureHandle object.
#'
#' @export
yield_curve_handle_GQL <- function(curve) {
  QuantLib::YieldTermStructureHandle(curve)
}


#' Extract tidy discount table from zero-rate nodes
#'
#' @param nodes A data frame with date and zero_rate columns.
#'
#' @return A tibble with zero rates, year fractions, and discount factors.
#'
#' @export
discount_table_GQL <- function(nodes) {
  stopifnot(is.data.frame(nodes))
  stopifnot(all(c("date", "zero_rate") %in% names(nodes)))

  first_date <- as.Date(nodes$date[[1]])

  tibble::tibble(
    date = as.character(nodes$date),
    zero_rate_input = as.numeric(nodes$zero_rate)
  ) |>
    dplyr::mutate(
      year_frac = as.numeric(as.Date(.data$date) - first_date) / 365,
      discount = dplyr::if_else(
        .data$year_frac > 0,
        exp(-.data$zero_rate_input * .data$year_frac),
        1
      ),
      implied_zero = dplyr::if_else(
        .data$year_frac > 0,
        -log(.data$discount) / .data$year_frac,
        0
      )
    )
}


#' Build a tidy OIS-style discount curve from zero-rate nodes
#'
#' @param nodes_tbl A data frame with date and zero_rate columns.
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return A list with curve and table.
#'
#' @export
ois_curve_GQL <- function(
    nodes_tbl,
    day_counter = "Actual365Fixed"
) {
  stopifnot(is.data.frame(nodes_tbl))
  stopifnot(all(c("date", "zero_rate") %in% names(nodes_tbl)))

  eval_date_GQL(nodes_tbl$date[[1]])

  curve <- build_discount_curve_GQL(
    nodes = nodes_tbl,
    day_counter = day_counter
  )

  out_tbl <- discount_table_GQL(nodes_tbl)

  list(
    curve = curve,
    table = out_tbl
  )
}


#' Example OIS curve nodes
#'
#' @return A tibble of sample zero-rate nodes.
#'
#' @export
example_ois_nodes_GQL <- function() {
  tibble::tribble(
    ~date,         ~zero_rate,
    "2026-04-12",  0.0030,
    "2026-05-12",  0.0032,
    "2026-07-12",  0.0034,
    "2026-10-12",  0.0038,
    "2027-04-12",  0.0045,
    "2028-04-12",  0.0065,
    "2029-04-12",  0.0080,
    "2031-04-12",  0.0105
  )
}


#' Run OIS curve example
#'
#' @return A tibble of discount factors.
#'
#' @export
ois_curve_example_GQL <- function() {
  nodes <- example_ois_nodes_GQL()
  result <- ois_curve_GQL(nodes)

  result$table
}


#' Build a bond discount curve
#'
#' @param settlement_date Settlement date.
#' @param fixing_days Fixing days for deposit helpers.
#' @param settlement_days Bond settlement days.
#'
#' @return A QuantLib yield term structure.
#'
#' @export
build_bond_discount_curve_GQL <- function(
    settlement_date = "2008-09-18",
    fixing_days = 3,
    settlement_days = 3
) {
  calendar <- QuantLib::UnitedStates("GovernmentBond")

  settlement_date_ql <- date_GQL(settlement_date)
  settlement_date_ql <- QuantLib::Calendar_adjust(
    calendar,
    settlement_date_ql
  )

  zc_quotes <- tibble::tibble(
    rate = c(0.0096, 0.0145, 0.0194),
    tenor_n = c(3, 6, 1),
    tenor_unit = c("Months", "Months", "Years")
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(
        .data$tenor_n,
        .data$tenor_unit,
        QuantLib::Period
      )
    )

  zc_bonds_day_counter <- QuantLib::Actual365Fixed()

  deposit_helpers_bond_curve <- purrr::pmap(
    list(zc_quotes$rate, zc_quotes$tenor),
    function(rate, tenor) {
      QuantLib::DepositRateHelper(
        quote_handle_GQL(rate),
        tenor,
        fixing_days,
        calendar,
        "ModifiedFollowing",
        TRUE,
        zc_bonds_day_counter
      )
    }
  )

  bond_quotes <- tibble::tibble(
    issue_date = c(
      "2005-03-15",
      "2005-06-15",
      "2006-06-30",
      "2002-11-15",
      "1987-05-15"
    ),
    maturity = c(
      "2010-08-31",
      "2011-08-31",
      "2013-08-31",
      "2018-08-15",
      "2038-05-15"
    ),
    coupon_rate = c(
      0.02375,
      0.04625,
      0.03125,
      0.04000,
      0.04500
    ),
    market_quote = c(
      100.390625,
      106.21875,
      100.59375,
      101.6875,
      102.140625
    )
  ) |>
    dplyr::mutate(
      issue_qldate = purrr::map(.data$issue_date, date_GQL),
      maturity_qldate = purrr::map(.data$maturity, date_GQL)
    )

  redemption <- 100.0

  bond_helpers <- purrr::pmap(
    list(
      bond_quotes$issue_qldate,
      bond_quotes$maturity_qldate,
      bond_quotes$coupon_rate,
      bond_quotes$market_quote
    ),
    function(issue_date, maturity_date, coupon_rate, market_quote) {
      schedule <- QuantLib::Schedule(
        issue_date,
        maturity_date,
        QuantLib::Period("Semiannual"),
        QuantLib::UnitedStates("GovernmentBond"),
        "Unadjusted",
        "Unadjusted",
        QuantLib::copyToR(
          QuantLib::DateGeneration(),
          "Backward"
        ),
        FALSE
      )

      QuantLib::FixedRateBondHelper(
        quote_handle_GQL(market_quote),
        settlement_days,
        100.0,
        schedule,
        coupon_rate,
        QuantLib::ActualActual("Bond"),
        "Unadjusted",
        redemption,
        issue_date
      )
    }
  )

  bond_instruments <- push_rate_helpers_GQL(
    c(deposit_helpers_bond_curve, bond_helpers)
  )

  term_structure_day_counter <- QuantLib::ActualActual("ISDA")

  QuantLib::PiecewiseFlatForward(
    settlement_date_ql,
    bond_instruments,
    term_structure_day_counter
  )
}


#' Build a deposit-swap forecasting curve
#'
#' @param settlement_date Settlement date.
#' @param fixing_days Fixing days for deposit helpers.
#'
#' @return A QuantLib yield term structure.
#'
#' @export
build_swap_curve_GQL <- function(
    settlement_date = "2008-09-18",
    fixing_days = 3
) {
  calendar <- QuantLib::UnitedStates("GovernmentBond")

  settlement_date_ql <- date_GQL(settlement_date)
  settlement_date_ql <- QuantLib::Calendar_adjust(
    calendar,
    settlement_date_ql
  )

  d_quotes <- tibble::tibble(
    rate = c(
      0.043375,
      0.031875,
      0.0320375,
      0.03385,
      0.0338125,
      0.0335125
    ),
    tenor_n = c(1, 1, 3, 6, 9, 1),
    tenor_unit = c(
      "Weeks",
      "Months",
      "Months",
      "Months",
      "Months",
      "Years"
    )
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(
        .data$tenor_n,
        .data$tenor_unit,
        QuantLib::Period
      )
    )

  s_quotes <- tibble::tibble(
    rate = c(0.0295, 0.0323, 0.0359, 0.0412, 0.0433),
    tenor_n = c(2, 3, 5, 10, 15),
    tenor_unit = c("Years", "Years", "Years", "Years", "Years")
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(
        .data$tenor_n,
        .data$tenor_unit,
        QuantLib::Period
      )
    )

  deposit_day_counter <- QuantLib::Actual360()

  depo_helpers_swap_curve <- purrr::pmap(
    list(d_quotes$rate, d_quotes$tenor),
    function(rate, tenor) {
      QuantLib::DepositRateHelper(
        quote_handle_GQL(rate),
        tenor,
        fixing_days,
        calendar,
        "ModifiedFollowing",
        TRUE,
        deposit_day_counter
      )
    }
  )

  sw_fixed_leg_frequency <- "Annual"
  sw_fixed_leg_convention <- "Unadjusted"
  sw_fixed_leg_day_counter <- QuantLib::Thirty360("European")
  sw_floating_leg_index <- QuantLib::Euribor6M()
  forward_start <- QuantLib::Period(1, "Days")

  swap_helpers <- purrr::pmap(
    list(s_quotes$rate, s_quotes$tenor),
    function(rate, tenor) {
      QuantLib::SwapRateHelper(
        quote_handle_GQL(rate),
        tenor,
        calendar,
        sw_fixed_leg_frequency,
        sw_fixed_leg_convention,
        sw_fixed_leg_day_counter,
        sw_floating_leg_index,
        QuantLib::QuoteHandle(),
        forward_start
      )
    }
  )

  depo_swap_instruments <- push_rate_helpers_GQL(
    c(depo_helpers_swap_curve, swap_helpers)
  )

  term_structure_day_counter <- QuantLib::ActualActual("ISDA")

  QuantLib::PiecewiseFlatForward(
    settlement_date_ql,
    depo_swap_instruments,
    term_structure_day_counter
  )
}
