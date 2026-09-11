# R/eonia_curve_bootstrap_GQL.R

#' Ametrano-Bianchetti EONIA benchmark quotes
#'
#' Return the exact market inputs used in chapter 8 of the QuantLib Python
#' Cookbook, based on figure 25 of Ametrano and Bianchetti (2013). Rates in
#' `rate_pct` are percentage quotes and `rate` contains decimal rates.
#'
#' @return A tibble containing deposit, spot-start OIS, dated OIS, and long OIS
#'   quotes.
#' @export
ametrano_bianchetti_eonia_quotes_GQL <- function() {
  tibble::tribble(
    ~quote_id, ~instrument_type, ~rate_pct, ~fixing_days, ~tenor_n, ~tenor_unit, ~start_date, ~end_date,
    "DEP-ON-0D", "deposit", 0.040, 0L, 1L, "Days", NA_character_, NA_character_,
    "DEP-ON-1D", "deposit", 0.040, 1L, 1L, "Days", NA_character_, NA_character_,
    "DEP-ON-2D", "deposit", 0.040, 2L, 1L, "Days", NA_character_, NA_character_,
    "OIS-1W", "spot_ois", 0.070, 2L, 1L, "Weeks", NA_character_, NA_character_,
    "OIS-2W", "spot_ois", 0.069, 2L, 2L, "Weeks", NA_character_, NA_character_,
    "OIS-3W", "spot_ois", 0.078, 2L, 3L, "Weeks", NA_character_, NA_character_,
    "OIS-1M", "spot_ois", 0.074, 2L, 1L, "Months", NA_character_, NA_character_,
    "OIS-ECB-1", "dated_ois", 0.046, NA_integer_, NA_integer_, NA_character_, "2013-01-16", "2013-02-13",
    "OIS-ECB-2", "dated_ois", 0.016, NA_integer_, NA_integer_, NA_character_, "2013-02-13", "2013-03-13",
    "OIS-ECB-3", "dated_ois", -0.007, NA_integer_, NA_integer_, NA_character_, "2013-03-13", "2013-04-10",
    "OIS-ECB-4", "dated_ois", -0.013, NA_integer_, NA_integer_, NA_character_, "2013-04-10", "2013-05-08",
    "OIS-ECB-5", "dated_ois", -0.014, NA_integer_, NA_integer_, NA_character_, "2013-05-08", "2013-06-12",
    "OIS-15M", "spot_ois", 0.002, 2L, 15L, "Months", NA_character_, NA_character_,
    "OIS-18M", "spot_ois", 0.008, 2L, 18L, "Months", NA_character_, NA_character_,
    "OIS-21M", "spot_ois", 0.021, 2L, 21L, "Months", NA_character_, NA_character_,
    "OIS-2Y", "spot_ois", 0.036, 2L, 2L, "Years", NA_character_, NA_character_,
    "OIS-3Y", "spot_ois", 0.127, 2L, 3L, "Years", NA_character_, NA_character_,
    "OIS-4Y", "spot_ois", 0.274, 2L, 4L, "Years", NA_character_, NA_character_,
    "OIS-5Y", "spot_ois", 0.456, 2L, 5L, "Years", NA_character_, NA_character_,
    "OIS-6Y", "spot_ois", 0.647, 2L, 6L, "Years", NA_character_, NA_character_,
    "OIS-7Y", "spot_ois", 0.827, 2L, 7L, "Years", NA_character_, NA_character_,
    "OIS-8Y", "spot_ois", 0.996, 2L, 8L, "Years", NA_character_, NA_character_,
    "OIS-9Y", "spot_ois", 1.147, 2L, 9L, "Years", NA_character_, NA_character_,
    "OIS-10Y", "spot_ois", 1.280, 2L, 10L, "Years", NA_character_, NA_character_,
    "OIS-11Y", "spot_ois", 1.404, 2L, 11L, "Years", NA_character_, NA_character_,
    "OIS-12Y", "spot_ois", 1.516, 2L, 12L, "Years", NA_character_, NA_character_,
    "OIS-15Y", "spot_ois", 1.764, 2L, 15L, "Years", NA_character_, NA_character_,
    "OIS-20Y", "spot_ois", 1.939, 2L, 20L, "Years", NA_character_, NA_character_,
    "OIS-25Y", "spot_ois", 2.003, 2L, 25L, "Years", NA_character_, NA_character_,
    "OIS-30Y", "spot_ois", 2.038, 2L, 30L, "Years", NA_character_, NA_character_
  ) |>
    dplyr::mutate(rate = rate_pct / 100) |>
    dplyr::relocate(rate, .after = rate_pct)
}


eonia_dated_ois_helper_GQL <- function(start_date, end_date, rate, eonia) {
  dated_constructor <- get0(
    "DatedOISRateHelper",
    envir = asNamespace("QuantLib"),
    inherits = FALSE
  )

  if (!is.null(dated_constructor)) {
    return(
      dated_constructor(
        date_GQL(start_date),
        date_GQL(end_date),
        quote_handle_GQL(rate),
        eonia
      )
    )
  }

  legacy_constructor <- get0(
    "OISRateHelper_forDates",
    envir = asNamespace("QuantLib"),
    inherits = FALSE
  )

  if (!is.null(legacy_constructor)) {
    return(
      legacy_constructor(
        date_GQL(start_date),
        date_GQL(end_date),
        quote_handle_GQL(rate),
        eonia
      )
    )
  }

  stop(
    "The installed QuantLib package exposes neither DatedOISRateHelper nor OISRateHelper_forDates.",
    call. = FALSE
  )
}


eonia_rate_helper_GQL <- function(
  instrument_type,
  rate,
  fixing_days,
  tenor_n,
  tenor_unit,
  start_date,
  end_date,
  eonia,
  calendar
) {
  if (instrument_type == "deposit") {
    return(
      QuantLib::DepositRateHelper(
        quote_handle_GQL(rate),
        QuantLib::Period(as.integer(tenor_n), as.character(tenor_unit)),
        as.integer(fixing_days),
        calendar,
        "Following",
        FALSE,
        QuantLib::Actual360()
      )
    )
  }

  if (instrument_type == "spot_ois") {
    return(
      QuantLib::OISRateHelper(
        as.integer(fixing_days),
        QuantLib::Period(as.integer(tenor_n), as.character(tenor_unit)),
        quote_handle_GQL(rate),
        eonia
      )
    )
  }

  if (instrument_type == "dated_ois") {
    return(
      eonia_dated_ois_helper_GQL(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        rate = rate,
        eonia = eonia
      )
    )
  }

  stop("Unsupported EONIA helper type: ", instrument_type, call. = FALSE)
}


eonia_quote_handle_vector_GQL <- function(values) {
  vector <- QuantLib::QuoteHandleVector()

  purrr::walk(
    as.numeric(values),
    function(value) {
      handle <- quote_handle_GQL(value)
      pushed <- tryCatch(
        {
          QuantLib::QuoteHandleVector_push_back(vector, handle)
          TRUE
        },
        error = function(e) FALSE
      )

      if (!pushed) {
        QuantLib::QuoteHandleVector_append(vector, handle)
      }
    }
  )

  vector
}


eonia_make_curve_GQL <- function(
  curve_type,
  calendar,
  helper_vector,
  day_counter,
  jump_discount_factors = NULL,
  jump_dates = NULL
) {
  constructor <- switch(curve_type,
    log_cubic_discount = QuantLib::PiecewiseLogCubicDiscount,
    flat_forward = QuantLib::PiecewiseFlatForward,
    stop("Unsupported EONIA curve type: ", curve_type, call. = FALSE)
  )

  has_jumps <- !is.null(jump_discount_factors) || !is.null(jump_dates)

  if (!has_jumps) {
    return(constructor(0L, calendar, helper_vector, day_counter))
  }

  if (is.null(jump_discount_factors) || is.null(jump_dates)) {
    stop("jump_discount_factors and jump_dates must be supplied together.", call. = FALSE)
  }

  if (length(jump_discount_factors) != length(jump_dates)) {
    stop("jump_discount_factors and jump_dates must have the same length.", call. = FALSE)
  }

  constructor(
    0L,
    calendar,
    helper_vector,
    day_counter,
    eonia_quote_handle_vector_GQL(jump_discount_factors),
    make_date_vector_GQL(as.character(jump_dates))
  )
}


#' Build an EONIA curve from market helpers
#'
#' Build the EONIA curve used in the Ametrano-Bianchetti example. The default
#' inputs reproduce chapter 8 of the QuantLib Python Cookbook exactly.
#'
#' @param quotes Market-quote tibble returned by
#'   `ametrano_bianchetti_eonia_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#' @param curve_type `"log_cubic_discount"` or `"flat_forward"`.
#' @param jump_discount_factors Optional multiplicative discount jumps.
#' @param jump_dates Optional jump dates in ISO format.
#' @param extrapolate Enable extrapolation beyond the final helper.
#'
#' @return A list containing the curve, curve handle, helper objects, input
#'   quotes, and conventions.
#' @export
build_eonia_curve_from_market_GQL <- function(
  quotes = ametrano_bianchetti_eonia_quotes_GQL(),
  evaluation_date = "2012-12-11",
  curve_type = c("log_cubic_discount", "flat_forward"),
  jump_discount_factors = NULL,
  jump_dates = NULL,
  extrapolate = TRUE
) {
  curve_type <- match.arg(curve_type)
  required_columns <- c(
    "quote_id", "instrument_type", "rate", "fixing_days", "tenor_n",
    "tenor_unit", "start_date", "end_date"
  )
  missing_columns <- setdiff(required_columns, names(quotes))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing EONIA quote columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  set_eval_date_GQL(date_GQL(evaluation_date))

  calendar <- QuantLib::TARGET()
  curve_day_counter <- QuantLib::Actual365Fixed()
  eonia <- QuantLib::Eonia()

  helpers <- purrr::pmap(
    list(
      instrument_type = quotes$instrument_type,
      rate = quotes$rate,
      fixing_days = quotes$fixing_days,
      tenor_n = quotes$tenor_n,
      tenor_unit = quotes$tenor_unit,
      start_date = quotes$start_date,
      end_date = quotes$end_date
    ),
    function(instrument_type,
             rate,
             fixing_days,
             tenor_n,
             tenor_unit,
             start_date,
             end_date) {
      eonia_rate_helper_GQL(
        instrument_type = instrument_type,
        rate = rate,
        fixing_days = fixing_days,
        tenor_n = tenor_n,
        tenor_unit = tenor_unit,
        start_date = start_date,
        end_date = end_date,
        eonia = eonia,
        calendar = calendar
      )
    }
  )

  helper_vector <- push_rate_helpers_GQL(helpers)
  curve <- eonia_make_curve_GQL(
    curve_type = curve_type,
    calendar = calendar,
    helper_vector = helper_vector,
    day_counter = curve_day_counter,
    jump_discount_factors = jump_discount_factors,
    jump_dates = jump_dates
  )

  if (isTRUE(extrapolate)) {
    tryCatch(
      QuantLib::TermStructure_enableExtrapolation(curve),
      error = function(e) NULL
    )
  }

  tryCatch(curve$discount(curve$maxDate()), error = function(e) NULL)

  list(
    curve = curve,
    curve_handle = QuantLib::YieldTermStructureHandle(curve),
    helpers = helpers,
    helper_vector = helper_vector,
    quotes = tibble::as_tibble(quotes),
    evaluation_date = as.character(evaluation_date),
    calendar = calendar,
    day_counter = curve_day_counter,
    curve_type = curve_type,
    jump_discount_factors = jump_discount_factors,
    jump_dates = jump_dates
  )
}


eonia_curve_dates_GQL <- function(curve) {
  curve_dates <- curve$dates()
  n_dates <- as.integer(curve_dates$size())

  purrr::map(
    seq_len(n_dates),
    function(i) curve_dates[i][[1L]]
  )
}

#' @export
eonia_curve_nodes_GQL <- function(curve) {
  dates_ql <- eonia_curve_dates_GQL(curve)
  n_dates <- length(dates_ql)

  if (n_dates == 0L) {
    return(
      tibble::tibble(
        node_date = as.Date(character()),
        node_rate = numeric()
      )
    )
  }

  if (n_dates == 1L) {
    return(
      tibble::tibble(
        node_date = as.Date(safe_iso_GQH(dates_ql[[1L]])),
        node_rate = NA_real_
      )
    )
  }

  interval_rates <- purrr::map_dbl(
    seq.int(2L, n_dates),
    function(i) {
      interest_rate <- curve$forwardRate(
        dates_ql[[i - 1L]],
        dates_ql[[i]],
        curve$dayCounter(),
        QuantLib::Compounding_Continuous_get()
      )

      safe_num_GQH(interest_rate$rate())
    }
  )

  tibble::tibble(
    node_date = as.Date(purrr::map_chr(dates_ql, safe_iso_GQH)),
    node_rate = c(interval_rates[[1L]], interval_rates)
  )
}


eonia_forward_rate_GQL <- function(
  curve,
  start_date,
  end_date,
  day_counter = QuantLib::Actual360()
) {
  tryCatch(
    curve$forwardRate(
      date_GQL(start_date),
      date_GQL(end_date),
      day_counter,
      QuantLib::Compounding_Simple_get()
    )$rate(),
    error = function(e) NA_real_
  )
}


eonia_helper_date_GQL <- function(
  helper,
  method = c("pillarDate", "latestDate")
) {
  method <- match.arg(method)

  tryCatch(
    switch(method,
      pillarDate = helper$pillarDate(),
      latestDate = helper$latestDate()
    ),
    error = function(e) NULL
  )
}


#' Validate EONIA curve quotes, spot rates, and forward rates
#'
#' Reprice every helper and report the discount factor, continuously compounded
#' spot rate, and one-business-day simple forward rate at each pillar date.
#'
#' @param curve_bundle Result from `build_eonia_curve_from_market_GQL()`.
#'
#' @return A list with `quote_repricing` and `curve_nodes` tibbles.
#' @export
eonia_curve_validation_GQL <- function(curve_bundle) {
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

  tbl_quote_repricing <-
    purrr::map2_dfr(
      seq_along(helpers),
      helpers,
      function(i, helper) {
        pillar_date <-
          eonia_helper_date_GQL(
            helper,
            "pillarDate"
          )

        if (is.null(pillar_date)) {
          pillar_date <-
            eonia_helper_date_GQL(
              helper,
              "latestDate"
            )
        }

        pillar_iso <-
          safe_iso_GQH(pillar_date)

        discount_factor <-
          if (is.na(pillar_iso)) {
            NA_real_
          } else {
            curve_discount_safe_GQL(
              curve,
              pillar_date
            )
          }

        year_fraction <-
          if (is.na(pillar_iso)) {
            NA_real_
          } else {
            safe_num_GQH(
              curve_day_counter$yearFraction(
                reference_date,
                pillar_date
              )
            )
          }

        zero_rate <-
          if (
            !is.na(year_fraction) &&
              year_fraction > 0 &&
              !is.na(discount_factor) &&
              discount_factor > 0
          ) {
            -log(discount_factor) /
              year_fraction
          } else {
            NA_real_
          }

        inst_fwd_rate <-
          if (is.na(pillar_iso)) {
            NA_real_
          } else {
            tryCatch(
              eonia_forward_rate_GQL(
                curve = curve,
                start_date = pillar_iso,
                end_date = pillar_iso
              ),
              error = function(e) NA_real_
            )
          }

        implied_quote <-
          tryCatch(
            safe_num_GQH(
              helper$impliedQuote()
            ),
            error = function(e) NA_real_
          )

        tibble::tibble(
          quote_id = quotes$quote_id[[i]],
          instrument_type =
            quotes$instrument_type[[i]],
          market_quote = quotes$rate[[i]],
          implied_quote = implied_quote,
          quote_error =
            implied_quote -
              quotes$rate[[i]],
          pillar_date = as.Date(pillar_iso),
          discount_factor = discount_factor,
          zero_rate = zero_rate,
          inst_fwd_rate =
            inst_fwd_rate
        )
      }
    )

  list(
    quote_repricing = tbl_quote_repricing,
    curve_nodes =
      eonia_curve_nodes_GQL(curve)
  )
}

eonia_forward_curve_from_nodes_GQL <- function(nodes, day_counter) {
  QuantLib::ForwardCurve(
    make_date_vector_GQL(as.character(nodes$node_date)),
    as.numeric(nodes$node_rate),
    day_counter
  )
}


#' Reproduce the QuantLib Cookbook EONIA bootstrap benchmark
#'
#' Build the initial log-cubic-discount and flat-forward curves, remove the
#' turn-of-year distortion from the flat-forward nodes, estimate the 2012
#' year-end jump, and rebuild the final log-cubic-discount curve with that jump.
#'
#' @param quotes Market-quote tibble returned by
#'   `ametrano_bianchetti_eonia_quotes_GQL()`.
#' @param evaluation_date Evaluation date in ISO format.
#'
#' @return A list containing all intermediate curves, validations, nodes, and
#'   the estimated year-end jump.
#' @export
#'
eonia_curve_benchmark_GQL <- function(
  quotes = ametrano_bianchetti_eonia_quotes_GQL(),
  evaluation_date = "2012-12-11"
) {
  lst_initial_bundle <-
    build_eonia_curve_from_market_GQL(
      quotes = quotes,
      evaluation_date = evaluation_date,
      curve_type = "log_cubic_discount"
    )

  lst_flat_bundle <-
    build_eonia_curve_from_market_GQL(
      quotes = quotes,
      evaluation_date = evaluation_date,
      curve_type = "flat_forward"
    )

  tbl_flat_nodes <-
    eonia_curve_nodes_GQL(
      lst_flat_bundle$curve
    )

  date_jump_node <- as.Date("2013-01-03")

  int_jump_index <-
    match(
      date_jump_node,
      tbl_flat_nodes$node_date
    )

  if (
    is.na(int_jump_index) ||
      int_jump_index <= 1L ||
      int_jump_index >= nrow(tbl_flat_nodes)
  ) {
    stop(
      paste(
        "Unable to identify the 2013-01-03",
        "flat-forward node required by the benchmark."
      ),
      call. = FALSE
    )
  }

  tbl_clean_nodes <-
    tbl_flat_nodes |>
    dplyr::mutate(
      node_rate = dplyr::if_else(
        dplyr::row_number() == int_jump_index,
        (
          tbl_flat_nodes$node_rate[[int_jump_index - 1L]] +
            tbl_flat_nodes$node_rate[[int_jump_index + 1L]]
        ) / 2,
        node_rate
      )
    )

  obj_curve_day_counter <-
    lst_flat_bundle$curve$dayCounter()

  obj_clean_forward_curve <-
    eonia_forward_curve_from_nodes_GQL(
      tbl_clean_nodes,
      obj_curve_day_counter
    )

  str_forward_start <- "2012-12-24"
  str_forward_end <- "2013-01-07"
  str_jump_start <- "2012-12-31"
  str_jump_end <- "2013-01-02"

  num_original_forward <-
    eonia_forward_rate_GQL(
      curve = lst_flat_bundle$curve,
      start_date = str_forward_start,
      end_date = str_forward_end
    )

  num_clean_forward <-
    eonia_forward_rate_GQL(
      curve = obj_clean_forward_curve,
      start_date = str_forward_start,
      end_date = str_forward_end
    )

  num_forward_time <-
    safe_num_GQH(
      obj_curve_day_counter$yearFraction(
        DateParser_parseISO_GQL(str_forward_start),
        DateParser_parseISO_GQL(str_forward_end)
      )
    )

  num_jump_time <-
    safe_num_GQH(
      obj_curve_day_counter$yearFraction(
        DateParser_parseISO_GQL(str_jump_start),
        DateParser_parseISO_GQL(str_jump_end)
      )
    )

  num_jump_rate <-
    (
      num_original_forward -
        num_clean_forward
    ) *
      num_forward_time /
      num_jump_time

  num_jump_discount_factor <-
    1 / (
      1 +
        num_jump_rate *
          num_jump_time
    )

  lst_final_bundle <-
    build_eonia_curve_from_market_GQL(
      quotes = quotes,
      evaluation_date = evaluation_date,
      curve_type = "log_cubic_discount",
      jump_discount_factors = num_jump_discount_factor,
      jump_dates = str_jump_start
    )

  lst_initial_validation <-
    eonia_curve_validation_GQL(
      lst_initial_bundle
    )

  lst_final_validation <-
    eonia_curve_validation_GQL(
      lst_final_bundle
    )

  tbl_jump_summary <-
    tibble::tibble(
      original_forward = num_original_forward,
      clean_forward = num_clean_forward,
      t12 = num_forward_time,
      jump_time = num_jump_time,
      jump_rate = num_jump_rate,
      jump_discount_factor = num_jump_discount_factor,
      jump_date = as.Date(str_jump_start)
    )

  list(
    quotes = quotes,
    initial = lst_initial_bundle,
    flat_forward = lst_flat_bundle,
    clean_forward_curve = obj_clean_forward_curve,
    final = lst_final_bundle,
    flat_nodes = tbl_flat_nodes,
    clean_nodes = tbl_clean_nodes,
    jump_summary = tbl_jump_summary,
    initial_validation = lst_initial_validation,
    final_validation = lst_final_validation
  )
}
