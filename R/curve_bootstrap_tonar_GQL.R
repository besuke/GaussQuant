# R/curve_bootstrap_tonar_GQL.R

#' BlueGamma TONAR OIS market quotes
#'
#' Return the JPY TONAR OIS swap rates observed on 2026-09-10. Rates in
#' `rate_pct` are percentage quotes and `rate` contains decimal rates.
#'
#' @return A tibble containing short- and long-tenor TONAR OIS quotes.
#' @export
bluegamma_tonar_quotes_GQL <- function() {
  tibble::tribble(
    ~quote_id, ~instrument_type, ~rate_pct, ~fixing_days, ~tenor_n, ~tenor_unit,
    "TONAR-1M", "spot_ois", 1.14, 2L, 1L, "Months",
    "TONAR-3M", "spot_ois", 1.22, 2L, 3L, "Months",
    "TONAR-6M", "spot_ois", 1.35, 2L, 6L, "Months",
    "TONAR-1Y", "spot_ois", 1.55, 2L, 1L, "Years",
    "TONAR-2Y", "spot_ois", 1.81, 2L, 2L, "Years",
    "TONAR-3Y", "spot_ois", 1.98, 2L, 3L, "Years",
    "TONAR-4Y", "spot_ois", 2.12, 2L, 4L, "Years",
    "TONAR-5Y", "spot_ois", 2.24, 2L, 5L, "Years",
    "TONAR-7Y", "spot_ois", 2.46, 2L, 7L, "Years",
    "TONAR-8Y", "spot_ois", 2.57, 2L, 8L, "Years",
    "TONAR-10Y", "spot_ois", 2.77, 2L, 10L, "Years",
    "TONAR-15Y", "spot_ois", 3.13, 2L, 15L, "Years",
    "TONAR-20Y", "spot_ois", 3.37, 2L, 20L, "Years",
    "TONAR-30Y", "spot_ois", 3.54, 2L, 30L, "Years"
  ) |>
    dplyr::mutate(rate = rate_pct / 100) |>
    dplyr::relocate(rate, .after = rate_pct)
}


#' Create a TONAR overnight index
#'
#' @param forwarding_curve_handle Optional QuantLib yield-term-structure
#'   handle.
#'
#' @return A QuantLib TONAR overnight-index object.
#' @export
tonar_GQL <- function(forwarding_curve_handle = NULL) {
  if (is.null(forwarding_curve_handle)) {
    return(QuantLib::Tonar())
  }

  QuantLib::Tonar(forwarding_curve_handle)
}


tonar_rate_helper_GQL <- function(
  instrument_type,
  rate,
  fixing_days,
  tenor_n,
  tenor_unit,
  tonar
) {
  if (identical(instrument_type, "deposit")) {
    return(
      QuantLib::DepositRateHelper(
        quote_handle_GQL(rate),
        tonar
      )
    )
  }

  if (instrument_type != "spot_ois") {
    stop(
      "Unsupported TONAR helper type: ",
      instrument_type,
      call. = FALSE
    )
  }

  QuantLib::OISRateHelper(
    as.integer(fixing_days),
    QuantLib::Period(
      as.integer(tenor_n),
      as.character(tenor_unit)
    ),
    quote_handle_GQL(rate),
    tonar
  )
}


tonar_make_curve_GQL <- function(
  curve_type,
  calendar,
  helper_vector,
  day_counter
) {
  constructor <- switch(
    curve_type,
    log_cubic_discount = QuantLib::PiecewiseLogCubicDiscount,
    log_linear_discount = QuantLib::PiecewiseLogLinearDiscount,
    flat_forward = QuantLib::PiecewiseFlatForward,
    stop(
      "Unsupported TONAR curve type: ",
      curve_type,
      call. = FALSE
    )
  )

  constructor(
    0L,
    calendar,
    helper_vector,
    day_counter
  )
}


#' Build a TONAR curve from JPY OIS market quotes
#'
#' @param quotes Market-quote tibble returned by
#'   `bluegamma_tonar_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#' @param curve_type `"log_cubic_discount"`, `"flat_forward"`, or
#'   `"log_linear_discount"`.
#' @param extrapolate Enable extrapolation beyond the final helper.
#'
#' @return A list containing the curve, curve handle, helper objects, input
#'   quotes, and conventions.
#' @export
build_tonar_curve_from_market_GQL <- function(
  quotes = bluegamma_tonar_quotes_GQL(),
  evaluation_date = "2026-09-10",
  curve_type = c("log_cubic_discount", "flat_forward", "log_linear_discount"),
  extrapolate = TRUE
) {
  curve_type <- match.arg(curve_type)
  required_columns <- c(
    "quote_id",
    "instrument_type",
    "rate",
    "fixing_days",
    "tenor_n",
    "tenor_unit"
  )
  missing_columns <- setdiff(required_columns, names(quotes))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing TONAR quote columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  set_eval_date_GQL(date_GQL(evaluation_date))

  calendar <- QuantLib::Japan()
  curve_day_counter <- QuantLib::Actual365Fixed()
  tonar <- tonar_GQL()

  helpers <- purrr::pmap(
    list(
      instrument_type = quotes$instrument_type,
      rate = quotes$rate,
      fixing_days = quotes$fixing_days,
      tenor_n = quotes$tenor_n,
      tenor_unit = quotes$tenor_unit
    ),
    function(
      instrument_type,
      rate,
      fixing_days,
      tenor_n,
      tenor_unit
    ) {
      tonar_rate_helper_GQL(
        instrument_type = instrument_type,
        rate = rate,
        fixing_days = fixing_days,
        tenor_n = tenor_n,
        tenor_unit = tenor_unit,
        tonar = tonar
      )
    }
  )

  helper_vector <- push_rate_helpers_GQL(helpers)
  curve <- tonar_make_curve_GQL(
    curve_type = curve_type,
    calendar = calendar,
    helper_vector = helper_vector,
    day_counter = curve_day_counter
  )

  if (isTRUE(extrapolate)) {
    tryCatch(
      QuantLib::TermStructure_enableExtrapolation(curve),
      error = function(e) NULL
    )
  }

  tryCatch(
    curve$discount(curve$maxDate()),
    error = function(e) NULL
  )

  list(
    curve = curve,
    curve_handle = QuantLib::YieldTermStructureHandle(curve),
    helpers = helpers,
    helper_vector = helper_vector,
    quotes = tibble::as_tibble(quotes),
    evaluation_date = as.character(evaluation_date),
    calendar = calendar,
    day_counter = curve_day_counter,
    index = tonar,
    curve_type = curve_type
  )
}


#' Validate a TONAR curve
#'
#' Reprice every helper and report the discount factor, continuously
#' compounded zero rate, and instantaneous forward rate at each pillar date.
#'
#' @param curve_bundle Result from `build_tonar_curve_from_market_GQL()`.
#'
#' @return A list with `quote_repricing` and `curve_nodes` tibbles.
#' @export
tonar_curve_validation_GQL <- function(curve_bundle) {
  stopifnot(is.list(curve_bundle))
  stopifnot(
    all(
      c("curve", "helpers", "quotes") %in%
        names(curve_bundle)
    )
  )

  curve <- curve_bundle$curve
  helpers <- curve_bundle$helpers
  quotes <- curve_bundle$quotes
  reference_date <- curve$referenceDate()
  curve_day_counter <- curve$dayCounter()

  tryCatch(
    curve$discount(curve$maxDate()),
    error = function(e) NULL
  )

  tbl_quote_repricing <- purrr::map2_dfr(
    seq_along(helpers),
    helpers,
    function(i, helper) {
      pillar_date <- rate_helper_date_GQL(
        helper,
        "pillarDate"
      )

      if (is.null(pillar_date)) {
        pillar_date <- rate_helper_date_GQL(
          helper,
          "latestDate"
        )
      }

      pillar_iso <- safe_iso_GQH(pillar_date)

      discount_factor <- if (is.na(pillar_iso)) {
        NA_real_
      } else {
        curve_discount_safe_GQL(
          curve,
          pillar_date
        )
      }

      year_fraction <- if (is.na(pillar_iso)) {
        NA_real_
      } else {
        safe_num_GQH(
          curve_day_counter$yearFraction(
            reference_date,
            pillar_date
          )
        )
      }

      zero_rate <- if (
        !is.na(year_fraction) &&
          year_fraction > 0 &&
          !is.na(discount_factor) &&
          discount_factor > 0
      ) {
        -log(discount_factor) / year_fraction
      } else {
        NA_real_
      }

      inst_fwd_rate <- if (is.na(pillar_iso)) {
        NA_real_
      } else {
        curve_forward_rate_GQL(
          curve = curve,
          start_date = pillar_iso,
          end_date = pillar_iso,
          day_counter = curve_day_counter
        )
      }

      implied_quote <- tryCatch(
        safe_num_GQH(helper$impliedQuote()),
        error = function(e) NA_real_
      )

      tibble::tibble(
        quote_id = quotes$quote_id[[i]],
        instrument_type = quotes$instrument_type[[i]],
        market_quote = quotes$rate[[i]],
        implied_quote = implied_quote,
        quote_error = implied_quote - quotes$rate[[i]],
        pillar_date = as.Date(pillar_iso),
        discount_factor = discount_factor,
        zero_rate = zero_rate,
        inst_fwd_rate = inst_fwd_rate
      )
    }
  )

  list(
    quote_repricing = tbl_quote_repricing,
    curve_nodes = curve_forward_nodes_GQL(curve)
  )
}


#' Build and validate the BlueGamma TONAR curve
#'
#' @param quotes Market-quote tibble returned by
#'   `bluegamma_tonar_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#' @param curve_type `"log_cubic_discount"` or `"flat_forward"`.
#'
#' @return A list containing the curve bundle and validation results.
#' @export
tonar_curve_benchmark_GQL <- function(
  quotes = bluegamma_tonar_quotes_GQL(),
  evaluation_date = "2026-09-10",
  curve_type = c("log_cubic_discount", "flat_forward")
) {
  curve_type <- match.arg(curve_type)

  lst_curve_bundle <- build_tonar_curve_from_market_GQL(
    quotes = quotes,
    evaluation_date = evaluation_date,
    curve_type = curve_type
  )

  lst_validation <- tonar_curve_validation_GQL(
    lst_curve_bundle
  )

  list(
    quotes = quotes,
    curve_bundle = lst_curve_bundle,
    validation = lst_validation
  )
}
