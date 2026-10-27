# R/curve_bootstrap_estr_GQL.R

#' BlueGamma ESTR OIS market quotes
#'
#' Return EUR ESTR OIS swap rates observed on 2026-09-10. Rates in
#' `rate_pct` are percentage quotes and `rate` contains decimal rates.
#'
#' @return A tibble containing ESTR OIS market quotes.
#' @export
bluegamma_estr_quotes_GQL <- function() {
  tibble::tribble(
    ~quote_id, ~instrument_type, ~rate_pct, ~fixing_days, ~tenor_n, ~tenor_unit,
    "ESTR-1M", "spot_ois", 2.43, 2L, 1L, "Months",
    "ESTR-3M", "spot_ois", 2.51, 2L, 3L, "Months",
    "ESTR-6M", "spot_ois", 2.69, 2L, 6L, "Months",
    "ESTR-1Y", "spot_ois", 2.97, 2L, 1L, "Years",
    "ESTR-2Y", "spot_ois", 3.13, 2L, 2L, "Years",
    "ESTR-3Y", "spot_ois", 3.15, 2L, 3L, "Years",
    "ESTR-4Y", "spot_ois", 3.16, 2L, 4L, "Years",
    "ESTR-5Y", "spot_ois", 3.17, 2L, 5L, "Years",
    "ESTR-7Y", "spot_ois", 3.21, 2L, 7L, "Years",
    "ESTR-8Y", "spot_ois", 3.24, 2L, 8L, "Years",
    "ESTR-10Y", "spot_ois", 3.29, 2L, 10L, "Years",
    "ESTR-15Y", "spot_ois", 3.40, 2L, 15L, "Years",
    "ESTR-20Y", "spot_ois", 3.44, 2L, 20L, "Years",
    "ESTR-30Y", "spot_ois", 3.34, 2L, 30L, "Years",
    "ESTR-50Y", "spot_ois", 3.05, 2L, 50L, "Years"
  ) |>
    dplyr::mutate(rate = rate_pct / 100) |>
    dplyr::relocate(rate, .after = rate_pct)
}


#' Create an ESTR overnight index
#'
#' @param forwarding_curve_handle Optional QuantLib yield-term-structure
#'   handle.
#'
#' @return A QuantLib ESTR overnight-index object.
#' @export
estr_GQL <- function(forwarding_curve_handle = NULL) {
  if (is.null(forwarding_curve_handle)) {
    return(QuantLib::Estr())
  }

  QuantLib::Estr(forwarding_curve_handle)
}


estr_rate_helper_GQL <- function(
  instrument_type,
  rate,
  fixing_days,
  tenor_n,
  tenor_unit,
  estr
) {
  if (instrument_type != "spot_ois") {
    stop(
      "Unsupported ESTR helper type: ",
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
    estr
  )
}


estr_make_curve_GQL <- function(
  curve_type,
  calendar,
  helper_vector,
  day_counter
) {
  constructor <- switch(
    curve_type,
    log_cubic_discount = QuantLib::PiecewiseLogCubicDiscount,
    flat_forward = QuantLib::PiecewiseFlatForward,
    stop(
      "Unsupported ESTR curve type: ",
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


#' Build an ESTR curve from EUR OIS market quotes
#'
#' @param quotes Market-quote tibble returned by
#'   `bluegamma_estr_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#' @param curve_type `"log_cubic_discount"` or `"flat_forward"`.
#' @param extrapolate Enable extrapolation beyond the final helper.
#'
#' @return A list containing the curve, curve handle, helper objects, input
#'   quotes, and conventions.
#' @export
build_estr_curve_from_market_GQL <- function(
  quotes = bluegamma_estr_quotes_GQL(),
  evaluation_date = "2026-09-10",
  curve_type = c("log_cubic_discount", "flat_forward"),
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
      "Missing ESTR quote columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  set_eval_date_GQL(date_GQL(evaluation_date))

  estr <- estr_GQL()
  calendar <- estr$fixingCalendar()
  curve_day_counter <- QuantLib::Actual360()

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
      estr_rate_helper_GQL(
        instrument_type = instrument_type,
        rate = rate,
        fixing_days = fixing_days,
        tenor_n = tenor_n,
        tenor_unit = tenor_unit,
        estr = estr
      )
    }
  )

  helper_vector <- push_rate_helpers_GQL(helpers)
  curve <- estr_make_curve_GQL(
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
    index = estr,
    curve_type = curve_type
  )
}


#' Validate a ESTR curve
#'
#' Reprice every helper and report the discount factor, continuously
#' compounded zero rate, and instantaneous forward rate at each pillar date.
#'
#' @param curve_bundle Result from `build_estr_curve_from_market_GQL()`.
#'
#' @return A list with `quote_repricing` and `curve_nodes` tibbles.
#' @export
estr_curve_validation_GQL <- function(curve_bundle) {
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


#' Build and validate the BlueGamma ESTR curve
#'
#' @param quotes Market-quote tibble returned by
#'   `bluegamma_estr_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#' @param curve_type `"log_cubic_discount"` or `"flat_forward"`.
#'
#' @return A list containing the curve bundle and validation results.
#' @export
estr_curve_benchmark_GQL <- function(
  quotes = bluegamma_estr_quotes_GQL(),
  evaluation_date = "2026-09-10",
  curve_type = c("log_cubic_discount", "flat_forward")
) {
  curve_type <- match.arg(curve_type)

  lst_curve_bundle <- build_estr_curve_from_market_GQL(
    quotes = quotes,
    evaluation_date = evaluation_date,
    curve_type = curve_type
  )

  lst_validation <- estr_curve_validation_GQL(
    lst_curve_bundle
  )

  list(
    quotes = quotes,
    curve_bundle = lst_curve_bundle,
    validation = lst_validation
  )
}
