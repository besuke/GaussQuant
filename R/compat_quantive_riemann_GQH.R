
# QuantiveRiemann compatibility helpers
# These GQH functions keep LagrangeFinance workflows working while core
# QuantLib pricing and analytics remain in the GaussQuant GQL layer.

.ir_msg_GQH <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    message(...)
  }
}

.safe_engine_set_GQH <- function(instrument, engine) {
  out <- tryCatch(
    instrument$setPricingEngine(engine),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(invisible(instrument))
  }

  tryCatch(
    QuantLib::Instrument_setPricingEngine(instrument, engine),
    error = function(e) stop("Could not set pricing engine: ", conditionMessage(e))
  )

  invisible(instrument)
}

.cf_amount_GQH <- function(cf) {
  tryCatch(
    cf$amount(),
    error = function(e) tryCatch(
      QuantLib::CashFlow_amount(cf),
      error = function(e2) NA_real_
    )
  )
}

.swap_leg_at_GQH <- function(swap, leg_index_zero_based) {
  tryCatch(
    swap$leg(as.integer(leg_index_zero_based)),
    error = function(e) tryCatch(
      QuantLib::Swap_leg(swap, as.integer(leg_index_zero_based)),
      error = function(e2) NULL
    )
  )
}

#' Safe accessor for a QuantLib cashflow leg
#'
#' @param leg_obj QuantLib leg object.
#' @param i_one_based One-based cashflow index.
#'
#' @return A QuantLib cashflow object, or NULL if not accessible.
#' @export
leg_cashflow_at_GQH <- function(leg_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1L)

  tryCatch(
    leg_obj$get(idx0),
    error = function(e1) {
      tryCatch(
        QuantLib::Leg___getitem__(leg_obj, idx0),
        error = function(e2) {
          tryCatch(leg_obj[[i_one_based]][[1]], error = function(e3) NULL)
        }
      )
    }
  )
}

#' Apply historical fixings to an index
#'
#' @param index_obj QuantLib index object.
#' @param fixings_tbl Data frame containing fixing dates and rates.
#' @param currency Optional currency filter.
#' @param instrument Optional instrument filter.
#'
#' @return The filtered fixing table, invisibly.
#' @export
ir_apply_fixings_GQH <- function(
    index_obj,
    fixings_tbl,
    currency = NULL,
    instrument = NULL
) {
  tbl <- tibble::as_tibble(fixings_tbl)

  if (!is.null(currency) && "currency" %in% names(tbl)) {
    tbl <- dplyr::filter(tbl, toupper(.data$currency) == toupper(currency))
  }

  if (!is.null(instrument) && "instrument" %in% names(tbl)) {
    tbl <- dplyr::filter(tbl, toupper(.data$instrument) == toupper(instrument))
  }

  date_col <- intersect(c("fixing_date", "date", "as_of_date", "trade_date"), names(tbl))[1]
  rate_col <- intersect(c("rate", "fixing", "fixing_value"), names(tbl))[1]

  if (is.na(date_col) || is.na(rate_col)) {
    stop("fixings_tbl must contain a fixing date column and a rate column")
  }

  purrr::walk2(
    tbl[[date_col]],
    tbl[[rate_col]],
    function(d, r) {
      d_ql <- date_GQL(d)
      r <- as.numeric(r)

      tryCatch(
        index_obj$addFixing(d_ql, r),
        error = function(e) tryCatch(
          QuantLib::Index_addFixing(index_obj, d_ql, r),
          error = function(e2) stop("Could not add fixing: ", conditionMessage(e2))
        )
      )
    }
  )

  invisible(tbl)
}

#' Print and return a swap summary
#'
#' @param trade_env Trade environment list containing a swap, or a QuantLib swap.
#' @param title Display title.
#'
#' @return Invisible list with NPV and fair rate.
#' @export
ir_show_swap_summary_GQH <- function(trade_env, title = "Swap summary") {
  swap <- if (is.list(trade_env) && !is.null(trade_env$swap)) {
    trade_env$swap
  } else {
    trade_env
  }

  npv <- tryCatch(swap_npv_GQL(swap), error = function(e) NA_real_)
  fair_rate <- tryCatch(swap_fair_rate_GQL(swap), error = function(e) NA_real_)

  message("========================================")
  message(title)
  message("========================================")
  message("NPV: ", npv)
  message("Fair rate: ", fair_rate)

  invisible(list(npv = npv, fair_rate = fair_rate))
}

#' Show fixing before/after comparison
#'
#' @param before_tbl Floating-leg table before fixings.
#' @param after_tbl Floating-leg table after fixings.
#' @param title Display title.
#' @param n Optional number of rows to print.
#'
#' @return Comparison tibble, invisibly.
#' @export
ir_show_fixing_before_after_GQH <- function(
    before_tbl,
    after_tbl,
    title = "Fixing before / after",
    n = NULL
) {
  message("========================================")
  message(title)
  message("========================================")

  cmp_tbl <- before_tbl |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select(
      row_id,
      dplyr::any_of(c("pay_date", "accrual_start", "accrual_end")),
      rate_before = dplyr::any_of("rate"),
      amount_before = dplyr::any_of("amount"),
      pv_before = dplyr::any_of("pv")
    ) |>
    dplyr::left_join(
      after_tbl |>
        dplyr::mutate(row_id = dplyr::row_number()) |>
        dplyr::select(
          row_id,
          rate_after = dplyr::any_of("rate"),
          amount_after = dplyr::any_of("amount"),
          pv_after = dplyr::any_of("pv")
        ),
      by = "row_id"
    )

  if (!is.null(n)) {
    cmp_tbl <- dplyr::slice_head(cmp_tbl, n = n)
  }

  print(cmp_tbl)
  invisible(cmp_tbl)
}

#' Build a fixing diagnostic table for a swap floating leg
#'
#' @param swap QuantLib swap.
#' @param curve QuantLib discount curve.
#' @param index QuantLib index.
#'
#' @return A tibble with fixing dates, fixing values, cashflow amounts, discount factors, and PVs.
#' @export
ir_fixing_table_GQH <- function(swap, curve, index) {
  float_leg <- .swap_leg_at_GQH(swap, 1L)

  if (is.null(float_leg)) {
    stop("Could not access floating leg")
  }

  n <- tryCatch(float_leg$size(), error = function(e) length(float_leg))

  tibble::tibble(i = seq_len(n)) |>
    dplyr::mutate(
      cf = purrr::map(.data$i, ~ leg_cashflow_at_GQH(float_leg, .x)),
      coupon = purrr::map(.data$cf, ~ tryCatch(QuantLib::as_floating_rate_coupon(.x), error = function(e) NULL)),
      fixing_date = purrr::map_chr(
        .data$cf,
        ~ iso_GQL(QuantLib::FloatingRateCoupon_fixingDate(QuantLib::as_floating_rate_coupon(.x)))
      ),
      fixing_value = purrr::map_dbl(
        .data$cf,
        ~ tryCatch(
          index$fixing(QuantLib::FloatingRateCoupon_fixingDate(QuantLib::as_floating_rate_coupon(.x))),
          error = function(e) NA_real_
        )
      ),
      pay_date = purrr::map_chr(.data$cf, ~ iso_GQL(QuantLib::CashFlow_date(.x))),
      amount = purrr::map_dbl(.data$cf, .cf_amount_GQH),
      df = purrr::map_dbl(
        .data$cf,
        ~ tryCatch(curve$discount(QuantLib::CashFlow_date(.x)), error = function(e) NA_real_)
      ),
      pv = .data$amount * .data$df
    ) |>
    dplyr::select(-cf, -coupon)
}

.business_dates_tbl_GQH <- function(start_date, end_date, calendar_obj) {
  dates <- seq.Date(
    from = as.Date(iso_GQL(date_GQL(start_date))),
    to = as.Date(iso_GQL(date_GQL(end_date))),
    by = "day"
  )

  tibble::tibble(
    accrual_date = dates
  ) |>
    dplyr::mutate(
      is_business_day = purrr::map_lgl(
        .data$accrual_date,
        function(d) {
          d_ql <- date_GQL(d)

          tryCatch(
            calendar_obj$isBusinessDay(d_ql),
            error = function(e) tryCatch(
              QuantLib::Calendar_isBusinessDay(calendar_obj, d_ql),
              error = function(e2) TRUE
            )
          )
        }
      )
    ) |>
    dplyr::filter(
      .data$is_business_day
    ) |>
    dplyr::transmute(
      accrual_date = as.character(.data$accrual_date)
    )
}

#' Daily forward decomposition for an OIS floating leg
#'
#' @param swap QuantLib OIS swap.
#' @param curve QuantLib discount curve.
#' @param index QuantLib overnight index.
#' @param eval_date Optional evaluation date.
#' @param calendar Optional calendar.
#' @param day_count Optional day counter.
#' @param notional Notional amount.
#'
#' @return A tibble with daily forward/fixing decomposition.
#' @export
ois_daily_forward_table_GQH <- function(
    swap,
    curve,
    index,
    eval_date = NULL,
    calendar = NULL,
    day_count = NULL,
    notional = 1e7
) {
  if (is.null(eval_date)) {
    eval_date <- QuantLib::Settings_instance()$evaluationDate()
  } else {
    eval_date <- date_GQL(eval_date)
  }

  if (is.null(calendar)) {
    calendar <- index$fixingCalendar()
  }

  if (is.null(day_count)) {
    day_count <- index$dayCounter()
  }

  leg_obj <- .swap_leg_at_GQH(swap, 1L)
  cf_obj <- leg_cashflow_at_GQH(leg_obj, 1L)
  cpn_obj <- QuantLib::as_floating_rate_coupon(cf_obj)

  accrual_start <- QuantLib::Coupon_accrualStartDate(cpn_obj)
  accrual_end <- QuantLib::Coupon_accrualEndDate(cpn_obj)

  eval_date_r <- as.Date(iso_GQL(eval_date))

  business_dates_tbl <- .business_dates_tbl_GQH(
    start_date = iso_GQL(accrual_start),
    end_date = iso_GQL(accrual_end),
    calendar_obj = calendar
  )

  business_dates_tbl |>
    dplyr::mutate(
      next_date = dplyr::lead(.data$accrual_date),
      fixing_date = .data$accrual_date
    ) |>
    dplyr::filter(!is.na(.data$next_date)) |>
    dplyr::mutate(
      fixing_date_r = as.Date(.data$fixing_date),
      next_date_r = as.Date(.data$next_date),
      days = as.integer(.data$next_date_r - .data$fixing_date_r),
      fixing_before_eval = .data$fixing_date_r < eval_date_r,
      fixing_on_eval = .data$fixing_date_r == eval_date_r,
      fixing_value = purrr::map_dbl(
        .data$fixing_date,
        ~ tryCatch(index$fixing(date_GQL(.x)), error = function(e) NA_real_)
      ),
      df_start = purrr::map_dbl(
        .data$fixing_date,
        ~ curve$discount(date_GQL(.x))
      ),
      df_end = purrr::map_dbl(
        .data$next_date,
        ~ curve$discount(date_GQL(.x))
      ),
      forward_rate_from_df = (.data$df_start / .data$df_end - 1) * 365 / .data$days,
      applied_rate = dplyr::case_when(
        .data$fixing_before_eval ~ .data$fixing_value,
        .data$fixing_on_eval ~ dplyr::coalesce(.data$fixing_value, .data$forward_rate_from_df),
        TRUE ~ .data$forward_rate_from_df
      ),
      amount = notional * .data$applied_rate * .data$days / 365
    ) |>
    dplyr::select(
      fixing_date,
      next_date,
      days,
      fixing_before_eval,
      fixing_on_eval,
      fixing_value,
      forward_rate_from_df,
      applied_rate,
      amount
    )
}

.ir_get_ois_convention_GQH <- function(currency, instrument) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))
  key <- paste(currency, instrument, sep = "::")

  switch(
    key,
    "JPY::TONA" = list(
      currency = "JPY",
      instrument = "TONA",
      calendar = QuantLib::Japan(),
      day_counter = QuantLib::Actual365Fixed(),
      fixing_days = 2L,
      settlement_days = 2L,
      index_builder = function(handle) {
        QuantLib::OvernightIndex(
          "TONA",
          2L,
          QuantLib::JPYCurrency(),
          QuantLib::Japan(),
          QuantLib::Actual365Fixed(),
          handle
        )
      }
    ),
    "USD::SOFR" = list(
      currency = "USD",
      instrument = "SOFR",
      calendar = QuantLib::UnitedStates("SOFR"),
      day_counter = QuantLib::Actual360(),
      fixing_days = 0L,
      settlement_days = 2L,
      index_builder = function(handle) {
        tryCatch(QuantLib::Sofr(handle), error = function(e) QuantLib::Sofr())
      }
    ),
    "EUR::ESTR" = list(
      currency = "EUR",
      instrument = "ESTR",
      calendar = QuantLib::TARGET(),
      day_counter = QuantLib::Actual360(),
      fixing_days = 0L,
      settlement_days = 2L,
      index_builder = function(handle) {
        tryCatch(QuantLib::Estr(handle), error = function(e) QuantLib::Estr())
      }
    ),
    "GBP::SONIA" = list(
      currency = "GBP",
      instrument = "SONIA",
      calendar = QuantLib::UnitedKingdom("Settlement"),
      day_counter = QuantLib::Actual365Fixed(),
      fixing_days = 0L,
      settlement_days = 0L,
      index_builder = function(handle) {
        tryCatch(QuantLib::Sonia(handle), error = function(e) QuantLib::Sonia())
      }
    ),
    stop("Unsupported OIS convention: ", key)
  )
}

.ir_make_ois_helper_GQH <- function(rate, tenor, settlement_days, index) {
  QuantLib::OISRateHelper(
    as.integer(settlement_days),
    period_GQL(tenor),
    quote_handle_GQL(rate),
    index
  )
}

.ir_make_deposit_helper_GQH <- function(rate, tenor, calendar, fixing_days, day_counter) {
  QuantLib::DepositRateHelper(
    quote_handle_GQL(rate),
    period_GQL(tenor),
    as.integer(fixing_days),
    calendar,
    "ModifiedFollowing",
    FALSE,
    day_counter
  )
}

.ir_normalize_curve_data_GQH <- function(curve_data) {
  tibble::as_tibble(curve_data) |>
    dplyr::transmute(
      kind = tolower(trimws(.data$kind)),
      tenor = toupper(trimws(.data$tenor)),
      rate = as.numeric(.data$rate)
    )
}

#' Build one OIS curve environment
#'
#' @param curve_data Data frame with kind, tenor, and rate.
#' @param trade_date Trade date.
#' @param currency Currency code.
#' @param instrument Instrument name.
#' @param verbose Whether to print progress messages.
#'
#' @return A curve environment list containing curve, curve_handle, index, and metadata.
#' @export
ir_make_ois_curve_GQH <- function(
    curve_data,
    trade_date,
    currency,
    instrument,
    verbose = TRUE
) {
  eval_date_GQL(trade_date)

  conv <- .ir_get_ois_convention_GQH(currency, instrument)

  trade_date_ql <- date_GQL(trade_date)
  settle_date <- advance_days_GQL(
    conv$calendar,
    trade_date_ql,
    conv$settlement_days
  )

  curve_handle <- QuantLib::RelinkableYieldTermStructureHandle()
  index_obj <- conv$index_builder(curve_handle)
  curve_data2 <- .ir_normalize_curve_data_GQH(curve_data)

  if (any(is.na(curve_data2$tenor) | curve_data2$tenor == "")) {
    stop("curve_data contains missing or empty tenor values")
  }

  if (any(is.na(curve_data2$rate))) {
    stop("curve_data contains non-numeric or missing rate values")
  }

  helper_list <- purrr::pmap(
    curve_data2,
    function(kind, tenor, rate) {
      if (kind == "ois") {
        .ir_make_ois_helper_GQH(
          rate = rate,
          tenor = tenor,
          settlement_days = conv$settlement_days,
          index = index_obj
        )
      } else if (kind == "depo") {
        .ir_make_deposit_helper_GQH(
          rate = rate,
          tenor = tenor,
          calendar = conv$calendar,
          fixing_days = conv$fixing_days,
          day_counter = conv$day_counter
        )
      } else {
        stop("Unsupported helper type for OIS curve: ", kind)
      }
    }
  )

  helper_vec <- QuantLib::RateHelperVector()
  purrr::walk(helper_list, ~ QuantLib::RateHelperVector_append(helper_vec, .x))

  curve_obj <- QuantLib::PiecewiseLogLinearDiscount(
    settle_date,
    helper_vec,
    conv$day_counter
  )

  tryCatch(QuantLib::TermStructure_enableExtrapolation(curve_obj), error = function(e) NULL)
  QuantLib::RelinkableYieldTermStructureHandle_linkTo(curve_handle, curve_obj)

  out <- list(
    trade_date = as.character(as.Date(trade_date)),
    currency = toupper(trimws(currency)),
    instrument = toupper(trimws(instrument)),
    settle_date = iso_GQL(settle_date),
    curve = curve_obj,
    curve_handle = curve_handle,
    index = index_obj
  )

  .ir_msg_GQH(
    verbose,
    "[ir_make_ois_curve_GQH] done: ",
    out$currency, "::", out$instrument,
    ", settle_date = ", out$settle_date
  )

  out
}

#' Build OIS curve environments from quote data
#'
#' @param quotes Data frame with as_of_date, currency, instrument, kind, tenor, and rate.
#' @param trade_date Optional trade date.
#' @param verbose Whether to print progress messages.
#'
#' @return Named list of curve environments.
#' @export
ir_build_ois_curve_envs_GQH <- function(
    quotes,
    trade_date = NULL,
    verbose = TRUE
) {
  required_cols <- c("as_of_date", "currency", "instrument", "kind", "tenor", "rate")
  missing_cols <- setdiff(required_cols, names(quotes))

  if (length(missing_cols) > 0) {
    stop("quotes is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  quotes2 <- tibble::as_tibble(quotes) |>
    dplyr::transmute(
      as_of_date = as.Date(.data$as_of_date),
      currency = toupper(trimws(.data$currency)),
      instrument = toupper(trimws(.data$instrument)),
      kind = tolower(trimws(.data$kind)),
      tenor = toupper(trimws(.data$tenor)),
      rate = as.numeric(.data$rate)
    )

  if (is.null(trade_date)) {
    trade_date <- unique(quotes2$as_of_date)

    if (length(trade_date) != 1L) {
      stop("trade_date is NULL but quotes contain multiple as_of_date values")
    }

    trade_date <- as.character(trade_date)
  }

  quotes3 <- quotes2 |>
    dplyr::filter(.data$as_of_date == as.Date(trade_date))

  keys <- quotes3 |>
    dplyr::distinct(.data$currency, .data$instrument)

  out <- purrr::pmap(
    keys,
    function(currency, instrument) {
      curve_data <- quotes3 |>
        dplyr::filter(
          .data$currency == .env$currency,
          .data$instrument == .env$instrument
        ) |>
        dplyr::select(.data$kind, .data$tenor, .data$rate)

      ir_make_ois_curve_GQH(
        curve_data = curve_data,
        trade_date = trade_date,
        currency = currency,
        instrument = instrument,
        verbose = verbose
      )
    }
  )

  names(out) <- paste(keys$currency, keys$instrument, sep = "::")
  out
}

.ir_get_ois_trade_convention_GQH <- function(currency, instrument) {
  conv <- .ir_get_ois_convention_GQH(currency, instrument)

  conv$fixed_leg_tenor <- period_years_GQL(1)
  conv$overnight_leg_tenor <- period_years_GQL(1)
  conv$fixed_day_counter <- conv$day_counter
  conv$payment_lag <- 0L

  conv
}

.ir_try_make_ois_trade_GQH <- function(
    swap_type,
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_counter,
    overnight_schedule,
    index,
    spread,
    payment_lag,
    effective,
    maturity
) {
  effective_chr <- as.character(effective)
  maturity_chr <- as.character(maturity)

  tenor_days <- as.integer(
    as.Date(maturity_chr) - as.Date(effective_chr)
  )

  if (is.na(tenor_days) || tenor_days <= 0L) {
    stop("OIS maturity must be after effective date.", call. = FALSE)
  }

  builder <- QuantLib::MakeOIS(
    swapTenor = QuantLib::Period(tenor_days, "Days"),
    overnightIndex = index,
    fixedRate = as.numeric(fixed_rate)
  )

  apply_builder <- function(builder_obj, builder_function, ...) {
    result <- tryCatch(
      builder_function(builder_obj, ...),
      error = function(e) NULL
    )

    if (is.null(result)) builder_obj else result
  }

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withEffectiveDate,
    date_GQL(effective_chr)
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withTerminationDate,
    date_GQL(maturity_chr)
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withNominal,
    as.numeric(nominal)
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withOvernightLegSpread,
    as.numeric(spread)
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withPaymentLag,
    as.integer(payment_lag)
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withFixedLegDayCount,
    fixed_day_counter
  )

  builder <- apply_builder(
    builder,
    QuantLib::MakeOIS_withType,
    swap_type
  )

  tryCatch(
    QuantLib::MakeOIS_makeOIS(builder),
    error = function(e) NULL
  )
}
#' Build an OIS swap trade from a curve environment
#'
#' @param curve_env Curve environment list.
#' @param effective Optional effective date.
#' @param maturity Maturity date.
#' @param nominal Nominal amount.
#' @param fixed_rate Fixed rate.
#' @param float_spread Floating spread.
#' @param swap_type QuantLib swap type.
#' @param fixed_bdc Business day convention.
#' @param date_rule Date generation rule.
#' @param eom End-of-month flag.
#' @param fixed_schedule_tenor Optional fixed leg schedule tenor.
#' @param overnight_schedule_tenor Optional overnight leg schedule tenor.
#' @param payment_lag Optional payment lag.
#' @param verbose Whether to print progress messages.
#'
#' @return Trade environment list.
#' @export
trade_ois_swap_GQH <- function(
    curve_env,
    effective = NULL,
    maturity,
    nominal = 1e6,
    fixed_rate,
    float_spread = 0.0,
    swap_type = QuantLib::Swap_Payer_get(),
    fixed_bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE,
    fixed_schedule_tenor = NULL,
    overnight_schedule_tenor = NULL,
    payment_lag = NULL,
    verbose = TRUE
) {
  if (is.null(curve_env$curve_handle)) {
    stop("curve_env must contain $curve_handle")
  }

  if (is.null(curve_env$currency) || is.null(curve_env$instrument)) {
    stop("curve_env must contain $currency and $instrument")
  }

  if (is.null(effective)) {
    if (is.null(curve_env$settle_date)) {
      stop("effective is NULL and curve_env does not contain $settle_date")
    }
    effective <- curve_env$settle_date
  }

  conv <- .ir_get_ois_trade_convention_GQH(
    currency = curve_env$currency,
    instrument = curve_env$instrument
  )

  fixed_schedule_tenor_obj <- if (is.null(fixed_schedule_tenor)) {
    conv$fixed_leg_tenor
  } else if (inherits(fixed_schedule_tenor, "Period")) {
    fixed_schedule_tenor
  } else {
    period_GQL(fixed_schedule_tenor)
  }

  overnight_schedule_tenor_obj <- if (is.null(overnight_schedule_tenor)) {
    conv$overnight_leg_tenor
  } else if (inherits(overnight_schedule_tenor, "Period")) {
    overnight_schedule_tenor
  } else {
    period_GQL(overnight_schedule_tenor)
  }

  payment_lag_use <- if (is.null(payment_lag)) {
    conv$payment_lag
  } else {
    as.integer(payment_lag)
  }

  fixed_schedule <- QuantLib::Schedule(
    date_GQL(effective),
    date_GQL(maturity),
    fixed_schedule_tenor_obj,
    conv$calendar,
    fixed_bdc,
    fixed_bdc,
    date_rule,
    eom
  )

  overnight_schedule <- QuantLib::Schedule(
    date_GQL(effective),
    date_GQL(maturity),
    overnight_schedule_tenor_obj,
    conv$calendar,
    fixed_bdc,
    fixed_bdc,
    date_rule,
    eom
  )

  index_obj <- conv$index_builder(curve_env$curve_handle)

  swap_obj <- .ir_try_make_ois_trade_GQH(
    swap_type = swap_type,
    nominal = nominal,
    fixed_schedule = fixed_schedule,
    fixed_rate = fixed_rate,
    fixed_day_counter = conv$fixed_day_counter,
    overnight_schedule = overnight_schedule,
    index = index_obj,
    spread = float_spread,
    payment_lag = payment_lag_use,
    effective = effective,
    maturity = maturity
  )

  if (is.null(swap_obj)) {
    stop(
      "Could not construct OIS swap with the QuantLib MakeOIS builder. ",
      "Your build may expose a different constructor signature."
    )
  }

  engine <- QuantLib::DiscountingSwapEngine(curve_env$curve_handle)
  .safe_engine_set_GQH(swap_obj, engine)

  out <- list(
    trade_type = "OIS_SWAP",
    swap = swap_obj,
    index = index_obj,
    curve_env = curve_env,
    fixed_schedule = fixed_schedule,
    overnight_schedule = overnight_schedule,
    effective = as.character(effective),
    maturity = as.character(maturity),
    nominal = nominal,
    fixed_rate = fixed_rate,
    float_spread = float_spread
  )

  .ir_msg_GQH(
    verbose,
    "[trade_ois_swap_GQH] done: ",
    curve_env$currency, "::", curve_env$instrument,
    " ", as.character(effective), " -> ", as.character(maturity)
  )

  out
}

#' Python-style OIS swap trade wrapper
#'
#' @export
trade_ois_swap_py_GQH <- function(
    curve_env,
    effective = NULL,
    maturity,
    notional = 1e6,
    fixed_rate,
    float_spread = 0.0,
    pay_receive = c("pay", "receive"),
    fixed_schedule_tenor = NULL,
    floating_schedule_tenor = NULL,
    pay_lag = NULL,
    fixed_bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE,
    verbose = TRUE
) {
  pay_receive <- match.arg(pay_receive)

  swap_type <- if (identical(pay_receive, "pay")) {
    QuantLib::Swap_Payer_get()
  } else {
    QuantLib::Swap_Receiver_get()
  }

  trade_ois_swap_GQH(
    curve_env = curve_env,
    effective = effective,
    maturity = maturity,
    nominal = notional,
    fixed_rate = fixed_rate,
    float_spread = float_spread,
    swap_type = swap_type,
    fixed_bdc = fixed_bdc,
    date_rule = date_rule,
    eom = eom,
    fixed_schedule_tenor = fixed_schedule_tenor,
    overnight_schedule_tenor = floating_schedule_tenor,
    payment_lag = pay_lag,
    verbose = verbose
  )
}

.curve_time_GQH <- function(curve, x) {
  x_ql <- if (is.character(x) || inherits(x, "Date")) {
    date_GQL(x)
  } else {
    x
  }

  tryCatch(
    curve$timeFromReference(x_ql),
    error = function(e) {
      ref <- curve$referenceDate()
      dc <- QuantLib::Actual365Fixed()
      dc$yearFraction(ref, x_ql)
    }
  )
}

.zero_rate_date_GQH <- function(curve, x) {
  x_ql <- if (is.character(x) || inherits(x, "Date")) {
    date_GQL(x)
  } else {
    x
  }

  tt <- .curve_time_GQH(curve, x_ql)

  if (is.na(tt) || tt <= 0) {
    return(NA_real_)
  }

  df <- tryCatch(
    curve$discount(x_ql),
    error = function(e) NA_real_
  )

  if (is.na(df) || df <= 0) {
    return(NA_real_)
  }

  -log(df) / tt
}

.ir_basis_time_table_GQH <- function(curve, basis_data) {
  tbl <- tibble::as_tibble(basis_data)

  spread_col <- intersect(c("spread", "basis", "rate"), names(tbl))[1]
  if (is.na(spread_col)) {
    stop("basis_data must contain one of: spread, basis, rate")
  }

  ref_date <- curve$referenceDate()

  tbl |>
    dplyr::transmute(
      tenor = toupper(trimws(.data$tenor)),
      spread = as.numeric(.data[[spread_col]])
    ) |>
    dplyr::mutate(
      target_date = purrr::map(.data$tenor, ~ ref_date + period_GQL(.x)),
      time = purrr::map_dbl(.data$target_date, ~ .curve_time_GQH(curve, .x))
    )
}

#' Make a simple interpolated basis curve environment
#'
#' @export
ir_make_basis_curve_GQH <- function(
    basis_data,
    base_curve_env,
    spread_label = "basis",
    verbose = TRUE
) {
  if (is.null(base_curve_env$curve)) {
    stop("base_curve_env must contain $curve")
  }

  curve_obj <- base_curve_env$curve
  basis_tbl <- .ir_basis_time_table_GQH(curve_obj, basis_data)

  if (any(is.na(basis_tbl$spread))) {
    stop("basis_data contains non-numeric or missing spread values")
  }

  if (nrow(basis_tbl) < 2) {
    stop("basis_data must contain at least 2 tenor points")
  }

  spread_fun <- stats::approxfun(
    x = basis_tbl$time,
    y = basis_tbl$spread,
    rule = 2
  )

  out <- list(
    trade_date = base_curve_env$trade_date,
    settle_date = if (!is.null(base_curve_env$settle_date)) base_curve_env$settle_date else NA_character_,
    currency = if (!is.null(base_curve_env$currency)) base_curve_env$currency else NA_character_,
    instrument = if (!is.null(base_curve_env$instrument)) base_curve_env$instrument else NA_character_,
    spread_label = spread_label,
    base_curve = curve_obj,
    base_curve_env = base_curve_env,
    basis_tbl = basis_tbl,
    spread_fun = spread_fun
  )

  class(out) <- c("ir_basis_curve_GQH", class(out))

  .ir_msg_GQH(verbose, "[ir_make_basis_curve_GQH] done: points = ", nrow(basis_tbl))

  out
}

.ir_basis_spread_GQH <- function(basis_env, x) {
  tt <- .curve_time_GQH(basis_env$base_curve, x)
  as.numeric(basis_env$spread_fun(tt))
}

.ir_basis_zero_rate_GQH <- function(basis_env, x) {
  base_zero <- .zero_rate_date_GQH(basis_env$base_curve, x)
  sprd <- .ir_basis_spread_GQH(basis_env, x)

  if (is.na(base_zero) || is.na(sprd)) {
    return(NA_real_)
  }

  base_zero + sprd
}

.ir_basis_discount_GQH <- function(basis_env, x) {
  tt <- .curve_time_GQH(basis_env$base_curve, x)
  z <- .ir_basis_zero_rate_GQH(basis_env, x)

  if (is.na(tt) || tt < 0 || is.na(z)) {
    return(NA_real_)
  }

  exp(-z * tt)
}

#' Build a basis curve table
#'
#' @export
ir_basis_table_GQH <- function(
    basis_env,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y", "20Y", "30Y")
) {
  ref_date <- basis_env$base_curve$referenceDate()

  tibble::tibble(
    tenor = tenors,
    target_date = purrr::map(.data$tenor, ~ ref_date + period_GQL(.x)),
    date = purrr::map_chr(.data$target_date, iso_GQL),
    spread = purrr::map_dbl(.data$target_date, ~ .ir_basis_spread_GQH(basis_env, .x)),
    base_zero = purrr::map_dbl(.data$target_date, ~ .zero_rate_date_GQH(basis_env$base_curve, .x)),
    basis_zero = purrr::map_dbl(.data$target_date, ~ .ir_basis_zero_rate_GQH(basis_env, .x)),
    basis_df = purrr::map_dbl(.data$target_date, ~ .ir_basis_discount_GQH(basis_env, .x))
  ) |>
    dplyr::select(.data$tenor, .data$date, .data$spread, .data$base_zero, .data$basis_zero, .data$basis_df)
}

.try_make_asset_swap_GQH <- function(
    bond,
    clean_price,
    ibor_index,
    spread,
    floating_schedule,
    floating_day_counter,
    par_asset_swap,
    maturity_date,
    pay_fixed_rate,
    gearing,
    non_par_repayment
) {
  candidates <- list(
    function() QuantLib::AssetSwap(
      pay_fixed_rate,
      bond,
      clean_price,
      ibor_index,
      spread,
      floating_schedule,
      floating_day_counter,
      par_asset_swap
    ),
    function() QuantLib::AssetSwap(
      pay_fixed_rate,
      bond,
      clean_price,
      ibor_index,
      spread,
      floating_schedule,
      floating_day_counter,
      par_asset_swap,
      gearing,
      non_par_repayment
    ),
    function() QuantLib::AssetSwap(
      pay_fixed_rate,
      bond,
      clean_price,
      ibor_index,
      spread,
      floating_schedule,
      floating_day_counter,
      par_asset_swap,
      maturity_date,
      gearing,
      non_par_repayment
    )
  )

  candidates |>
    purrr::map(
      ~ tryCatch(
        .x(),
        error = function(e) NULL
      )
    ) |>
    purrr::compact() |>
    purrr::pluck(
      1,
      .default = NULL
    )
}

#' Asset swap analysis for a fixed-rate bond
#'
#' @export
asset_swap_analysis_GQH <- function(
    bond,
    clean_price,
    ibor_index,
    spread,
    settlement_date,
    maturity_date,
    calendar,
    floating_schedule_frequency,
    payment_convention,
    date_generation,
    end_of_month = FALSE,
    floating_day_counter,
    pay_fixed_rate = TRUE,
    par_asset_swap = TRUE,
    discount_curve_handle,
    gearing = 1.0,
    non_par_repayment = 100.0
) {
  settlement_date <- date_GQL(settlement_date)
  maturity_date <- date_GQL(maturity_date)

  floating_schedule <- QuantLib::Schedule(
    settlement_date,
    maturity_date,
    floating_schedule_frequency,
    calendar,
    payment_convention,
    payment_convention,
    date_generation,
    end_of_month
  )

  asset_swap_obj <- .try_make_asset_swap_GQH(
    bond = bond,
    clean_price = clean_price,
    ibor_index = ibor_index,
    spread = spread,
    floating_schedule = floating_schedule,
    floating_day_counter = floating_day_counter,
    par_asset_swap = par_asset_swap,
    maturity_date = maturity_date,
    pay_fixed_rate = pay_fixed_rate,
    gearing = gearing,
    non_par_repayment = non_par_repayment
  )

  if (is.null(asset_swap_obj)) {
    return(
      tibble::tibble(
        metric = c("fair_spread", "fair_clean_price", "status"),
        value = list(
          NA_real_,
          NA_real_,
          "AssetSwap constructor failed in this SWIG build"
        )
      )
    )
  }

  asset_swap_engine <- QuantLib::DiscountingSwapEngine(discount_curve_handle)
  .safe_engine_set_GQH(asset_swap_obj, asset_swap_engine)

  tibble::tibble(
    metric = c("fair_spread", "fair_clean_price", "status"),
    value = list(
      tryCatch(QuantLib::AssetSwap_fairSpread(asset_swap_obj), error = function(e) tryCatch(asset_swap_obj$fairSpread(), error = function(e2) NA_real_)),
      tryCatch(QuantLib::AssetSwap_fairCleanPrice(asset_swap_obj), error = function(e) tryCatch(asset_swap_obj$fairCleanPrice(), error = function(e2) NA_real_)),
      "ok"
    )
  )
}
