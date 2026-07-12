# swap_analytics.R

#' Swap NPV
#'
#' @param swap A QuantLib Swap object.
#'
#' @return Swap NPV.
#' @export
swap_npv_GQL <- function(swap) {
  swap$NPV()
}

#' Swap leg NPV
#'
#' @param swap A QuantLib Swap object.
#' @param leg_no Leg number. QuantLib uses 0-based leg indexing.
#'
#' @return Swap leg NPV.
#' @export
swap_leg_npv_GQL <- function(swap, leg_no) {
  swap$legNPV(as.integer(leg_no))
}

#' Swap fixed leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Fixed leg NPV.
#' @export
swap_fixed_leg_npv_GQL <- function(swap) {
  swap_leg_npv_GQL(swap, 0L)
}

#' Swap floating leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Floating leg NPV.
#' @export
swap_floating_leg_npv_GQL <- function(swap) {
  swap_leg_npv_GQL(swap, 1L)
}

#' Swap fair fixed rate
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Fair fixed rate. Returns NA if unavailable.
#' @export
swap_fair_rate_GQL <- function(swap) {
  tryCatch(
    swap$fairRate(),
    error = function(e) NA_real_
  )
}

#' Swap fair spread
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Fair spread. Returns NA if unavailable.
#' @export
swap_fair_spread_GQL <- function(swap) {
  tryCatch(
    swap$fairSpread(),
    error = function(e) NA_real_
  )
}

#' Swap fixed leg cashflow table
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble of fixed leg cashflows.
#' @export
swap_fixed_leg_table_GQL <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$fixedLeg())
}

#' Swap floating leg cashflow table
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble of floating leg cashflows.
#' @export
swap_floating_leg_table_GQL <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$floatingLeg())
}

#' Swap summary
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble summarising swap valuation measures.
#' @export
swap_summary_GQL <- function(swap) {
  use_quantlib_GQH()
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    npv = swap_npv_GQL(swap),
    fixed_leg_npv = swap_fixed_leg_npv_GQL(swap),
    floating_leg_npv = swap_floating_leg_npv_GQL(swap),
    fair_rate = swap_fair_rate_GQL(swap),
    fair_spread = swap_fair_spread_GQL(swap)
  )
}

#' OIS overnight leg cashflow table
#'
#' @param swap A QuantLib OIS-like object.
#'
#' @return A tibble of overnight leg cashflows.
#' @export
ois_overnight_leg_table_GQL <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$overnightLeg())
}

#' Summarise OIS cashflow legs
#'
#' @param fixed_leg Fixed leg cashflow table.
#' @param overnight_leg Overnight leg cashflow table.
#'
#' @return A tibble summarising OIS cashflow legs.
#' @export
ois_summary_GQL <- function(fixed_leg, overnight_leg) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  pick_col <- function(x, candidates) {
    hit <- intersect(candidates, names(x))

    if (length(hit) == 0) {
      NA_character_
    } else {
      hit[[1]]
    }
  }

  safe_date_range <- function(x, date_col) {
    if (is.na(date_col) || nrow(x) == 0) {
      return(list(first = as.Date(NA), last = as.Date(NA)))
    }

    d <- suppressWarnings(as.Date(as.character(x[[date_col]])))

    if (all(is.na(d))) {
      return(list(first = as.Date(NA), last = as.Date(NA)))
    }

    list(
      first = min(d, na.rm = TRUE),
      last = max(d, na.rm = TRUE)
    )
  }

  summarise_leg <- function(x, leg_name) {
    amount_col <- pick_col(
      x,
      c("amount", "Amount", "cashflow_amount", "cashflow")
    )

    date_col <- pick_col(
      x,
      c("date", "Date", "payment_date", "pay_date")
    )

    date_range <- safe_date_range(x, date_col)

    total_amount <- if (is.na(amount_col)) {
      NA_real_
    } else {
      sum(as.numeric(x[[amount_col]]), na.rm = TRUE)
    }

    tibble::tibble(
      leg = leg_name,
      cashflow_count = nrow(x),
      first_payment_date = date_range$first,
      last_payment_date = date_range$last,
      total_amount = total_amount
    )
  }

  dplyr::bind_rows(
    summarise_leg(fixed_leg, "fixed_leg"),
    summarise_leg(overnight_leg, "overnight_leg")
  )
}

# R/swap_analytics.R




# R/swap_analytics.R

cashflow_pick_col_GQL <- function(x, candidates) {
  hit <- intersect(candidates, names(x))

  if (length(hit) == 0) {
    NA_character_
  } else {
    hit[[1]]
  }
}


cashflow_standardise_GQL <- function(x) {
  requireNamespace("dplyr", quietly = TRUE)

  date_col <- cashflow_pick_col_GQL(
    x,
    c("payment_date", "pay_date", "date", "Date")
  )

  amount_col <- cashflow_pick_col_GQL(
    x,
    c("amount", "Amount", "cashflow_amount", "cashflow")
  )

  if (is.na(date_col)) {
    x$payment_date <- as.Date(NA)
  } else {
    x$payment_date <- suppressWarnings(
      as.Date(as.character(x[[date_col]]))
    )
  }

  if (is.na(amount_col)) {
    x$amount <- NA_real_
  } else {
    x$amount <- as.numeric(x[[amount_col]])
  }

  x |>
    dplyr::relocate(.data$payment_date, .data$amount)
}


#' Build cashflow schedule table from a QuantLib leg
#'
#' @param leg A QuantLib Leg object.
#' @param leg_name Leg name.
#' @param leg_no Leg number. QuantLib uses 0-based leg indexing.
#'
#' @return A tibble containing cashflows for one leg.
#'
#' @export
leg_cashflow_schedule_GQL <- function(
    leg,
    leg_name,
    leg_no = NA_integer_
) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  qlg_leg_to_cashflow_tbl(leg) |>
    cashflow_standardise_GQL() |>
    dplyr::mutate(
      leg = leg_name,
      leg_no = as.integer(leg_no)
    ) |>
    dplyr::relocate(.data$leg, .data$leg_no)
}


#' Build cashflow schedule table from swap legs
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble containing fixed and floating leg cashflows.
#'
#' @export
swap_cashflow_schedule_GQL <- function(swap) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  fixed_leg <- leg_cashflow_schedule_GQL(
    leg = swap$fixedLeg(),
    leg_name = "fixed",
    leg_no = 0L
  )

  floating_leg <- leg_cashflow_schedule_GQL(
    leg = swap$floatingLeg(),
    leg_name = "floating",
    leg_no = 1L
  )

  dplyr::bind_rows(
    fixed_leg,
    floating_leg
  )
}


#' Build OIS cashflow schedule table from fixed and overnight legs
#'
#' @param swap A QuantLib OIS-like object.
#'
#' @return A tibble containing fixed and overnight leg cashflows.
#'
#' @export
ois_cashflow_schedule_GQL <- function(swap) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  fixed_leg <- leg_cashflow_schedule_GQL(
    leg = swap$fixedLeg(),
    leg_name = "fixed",
    leg_no = 0L
  )

  overnight_leg <- leg_cashflow_schedule_GQL(
    leg = swap$overnightLeg(),
    leg_name = "overnight",
    leg_no = 1L
  )

  dplyr::bind_rows(
    fixed_leg,
    overnight_leg
  )
}







# R/swap_analytics.R

curve_discount_factor_GQL <- function(
    curve,
    date
) {
  use_quantlib_GQH()

  if (is.na(date)) {
    return(NA_real_)
  }

  qd <- qlg_date(as.character(date))

  tryCatch(
    curve$discount(qd),
    error = function(e) NA_real_
  )
}


day_count_fraction_GQL <- function(
    start_date,
    end_date,
    day_counter = "Actual365Fixed"
) {
  use_quantlib_GQH()

  if (is.na(start_date) || is.na(end_date)) {
    return(NA_real_)
  }

  dc <- qlg_day_counter(day_counter)
  ql_start <- qlg_date(as.character(start_date))
  ql_end <- qlg_date(as.character(end_date))

  tryCatch(
    dc$yearFraction(ql_start, ql_end),
    error = function(e) {
      as.numeric(as.Date(end_date) - as.Date(start_date)) / 365
    }
  )
}


#' Apply discount factors to cashflow schedule
#'
#' @param cashflows A cashflow schedule tibble.
#' @param discount_curve A QuantLib yield curve or yield curve handle.
#' @param payment_date_col Payment date column name.
#' @param amount_col Amount column name.
#'
#' @return A tibble with discount_factor and present_value.
#'
#' @export
apply_discount_factors_GQL <- function(
    cashflows,
    discount_curve,
    payment_date_col = "payment_date",
    amount_col = "amount"
) {
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(cashflows))
  stopifnot(payment_date_col %in% names(cashflows))
  stopifnot(amount_col %in% names(cashflows))

  payment_dates <- suppressWarnings(
    as.Date(as.character(cashflows[[payment_date_col]]))
  )

  discount_factors <- vapply(
    seq_along(payment_dates),
    function(i) {
      curve_discount_factor_GQL(
        curve = discount_curve,
        date = payment_dates[i]
      )
    },
    numeric(1)
  )

  cashflows |>
    dplyr::mutate(
      discount_factor = discount_factors,
      present_value = as.numeric(.data[[amount_col]]) * .data$discount_factor
    )
}


#' Apply fixing data to cashflow schedule
#'
#' @param cashflows A cashflow schedule tibble.
#' @param fixings A fixing tibble with fixing_date and fixing columns.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with fixing_value and is_fixing_known.
#'
#' @export
apply_fixings_GQL <- function(
    cashflows,
    fixings,
    evaluation_date = NULL
) {
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(cashflows))
  stopifnot(is.data.frame(fixings))
  stopifnot(all(c("fixing_date", "fixing") %in% names(fixings)))

  if (is.null(evaluation_date)) {
    evaluation_date <- Sys.Date()
  } else {
    evaluation_date <- as.Date(evaluation_date)
  }

  out <- cashflows

  if (!("fixing_date" %in% names(out))) {
    out$fixing_date <- as.Date(NA)
  } else {
    out$fixing_date <- suppressWarnings(
      as.Date(as.character(out$fixing_date))
    )
  }

  fixings_tbl <- fixings |>
    dplyr::mutate(
      fixing_date = as.Date(.data$fixing_date),
      fixing_value = as.numeric(.data$fixing)
    ) |>
    dplyr::select(-.data$fixing)

  join_cols <- if (
    "index" %in% names(out) &&
    "index" %in% names(fixings_tbl)
  ) {
    c("index", "fixing_date")
  } else {
    "fixing_date"
  }

  out |>
    dplyr::left_join(
      fixings_tbl,
      by = join_cols
    ) |>
    dplyr::mutate(
      is_fixing_known = !is.na(.data$fixing_value) &
        !is.na(.data$fixing_date) &
        .data$fixing_date <= evaluation_date
    )
}


#' Apply forward rates to unfixed cashflows
#'
#' @param cashflows A cashflow schedule tibble.
#' @param forecast_curve A QuantLib yield curve or yield curve handle.
#' @param accrual_start_col Accrual start date column name.
#' @param accrual_end_col Accrual end date column name.
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return A tibble with year_fraction and forward_rate.
#'
#' @export
apply_forward_rates_GQL <- function(
    cashflows,
    forecast_curve,
    accrual_start_col = "accrual_start_date",
    accrual_end_col = "accrual_end_date",
    day_counter = "Actual365Fixed"
) {
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(cashflows))

  out <- cashflows

  if (!(accrual_start_col %in% names(out))) {
    out[[accrual_start_col]] <- as.Date(NA)
  }

  if (!(accrual_end_col %in% names(out))) {
    out[[accrual_end_col]] <- as.Date(NA)
  }

  accrual_start <- suppressWarnings(
    as.Date(as.character(out[[accrual_start_col]]))
  )

  accrual_end <- suppressWarnings(
    as.Date(as.character(out[[accrual_end_col]]))
  )

  year_fractions <- vapply(
    seq_along(accrual_start),
    function(i) {
      day_count_fraction_GQL(
        start_date = accrual_start[i],
        end_date = accrual_end[i],
        day_counter = day_counter
      )
    },
    numeric(1)
  )

  forward_rates <- vapply(
    seq_along(accrual_start),
    function(i) {
      yf <- year_fractions[i]

      if (
        is.na(accrual_start[i]) ||
        is.na(accrual_end[i]) ||
        is.na(yf) ||
        yf <= 0
      ) {
        return(NA_real_)
      }

      df_start <- curve_discount_factor_GQL(
        curve = forecast_curve,
        date = accrual_start[i]
      )

      df_end <- curve_discount_factor_GQL(
        curve = forecast_curve,
        date = accrual_end[i]
      )

      if (
        is.na(df_start) ||
        is.na(df_end) ||
        df_end == 0
      ) {
        return(NA_real_)
      }

      (df_start / df_end - 1) / yf
    },
    numeric(1)
  )

  out |>
    dplyr::mutate(
      year_fraction = year_fractions,
      forward_rate = forward_rates
    )
}


#' Value cashflow schedule with fixings, forwards, and discount factors
#'
#' @param cashflows A cashflow schedule tibble.
#' @param discount_curve A QuantLib discount curve or handle.
#' @param forecast_curve Optional QuantLib forecast curve or handle.
#' @param fixings Optional fixing tibble with fixing_date and fixing columns.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with fixing, forward, discount factor, and PV columns.
#'
#' @export
value_cashflow_schedule_GQL <- function(
    cashflows,
    discount_curve,
    forecast_curve = NULL,
    fixings = NULL,
    evaluation_date = NULL
) {
  requireNamespace("dplyr", quietly = TRUE)

  out <- cashflows

  if (!is.null(fixings)) {
    out <- apply_fixings_GQL(
      cashflows = out,
      fixings = fixings,
      evaluation_date = evaluation_date
    )
  }

  if (!is.null(forecast_curve)) {
    out <- apply_forward_rates_GQL(
      cashflows = out,
      forecast_curve = forecast_curve
    )
  }

  out <- apply_discount_factors_GQL(
    cashflows = out,
    discount_curve = discount_curve
  )

  out
}


#' Summarise cashflows by leg
#'
#' @param cashflows A cashflow schedule tibble.
#'
#' @return A tibble summarising cashflows by leg.
#'
#' @export
cashflow_leg_summary_GQL <- function(cashflows) {
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(cashflows))

  if (!("leg" %in% names(cashflows))) {
    cashflows$leg <- NA_character_
  }

  if (!("payment_date" %in% names(cashflows))) {
    cashflows$payment_date <- as.Date(NA)
  }

  if (!("amount" %in% names(cashflows))) {
    cashflows$amount <- NA_real_
  }

  if (!("present_value" %in% names(cashflows))) {
    cashflows$present_value <- NA_real_
  }

  cashflows |>
    dplyr::group_by(.data$leg) |>
    dplyr::summarise(
      cashflow_count = dplyr::n(),
      first_payment_date = suppressWarnings(
        min(as.Date(as.character(.data$payment_date)), na.rm = TRUE)
      ),
      last_payment_date = suppressWarnings(
        max(as.Date(as.character(.data$payment_date)), na.rm = TRUE)
      ),
      total_amount = sum(as.numeric(.data$amount), na.rm = TRUE),
      total_present_value = sum(as.numeric(.data$present_value), na.rm = TRUE),
      .groups = "drop"
    )
}

#' Build an OIS cashflow schedule from trade data
#'
#'
#' @param trade A one-row data frame containing OIS trade fields.
#' @param forecast_handle QuantLib forecast curve handle.
#'
#' @return A tibble containing fixed and overnight leg cashflows.
#'
#' @export
ois_cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle
) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  swap <- make_ois_from_trade_GQL(
    trade = trade,
    forecast_handle = forecast_handle
  )

  out <- ois_cashflow_schedule_GQL(swap)

  if ("trade_id" %in% names(trade)) {
    out <- out |>
      dplyr::mutate(
        trade_id = as.character(trade$trade_id[[1]])
      ) |>
      dplyr::relocate(.data$trade_id)
  }

  out
}


#' Value OIS cashflow schedule from trade data
#'
#' @param trade A one-row data frame containing OIS trade fields.
#' @param forecast_handle QuantLib forecast curve handle.
#' @param discount_curve QuantLib discount curve or handle.
#' @param fixings Optional fixing tibble.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with cashflows, discount factors, fixings, forwards, and PVs.
#'
#' @export
value_ois_cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle,
    discount_curve,
    fixings = NULL,
    evaluation_date = NULL
) {
  use_quantlib_GQH()

  cashflows <- ois_cashflow_schedule_from_trade_GQL(
    trade = trade,
    forecast_handle = forecast_handle
  )

  value_cashflow_schedule_GQL(
    cashflows = cashflows,
    discount_curve = discount_curve,
    forecast_curve = forecast_handle,
    fixings = fixings,
    evaluation_date = evaluation_date
  )
}

# R/swap_analytics.R

#' Build VanillaSwap cashflow schedule from trade data
#'
#' @param trade A one-row data frame containing VanillaSwap trade fields.
#' @param forecast_handle Forecast curve handle.
#'
#' @return A tibble containing fixed and floating leg cashflows.
#'
#' @export
swap_cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle = NULL
) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  swap <- make_vanilla_swap_from_trade_GQL(
    trade = trade,
    forecast_handle = forecast_handle
  )

  out <- swap_cashflow_schedule_GQL(swap)

  if ("trade_id" %in% names(trade)) {
    out <- out |>
      dplyr::mutate(
        trade_id = as.character(trade$trade_id[[1]])
      ) |>
      dplyr::relocate(.data$trade_id)
  }

  out
}


#' Value VanillaSwap cashflow schedule from trade data
#'
#' @param trade A one-row data frame containing VanillaSwap trade fields.
#' @param forecast_handle Forecast curve handle.
#' @param discount_curve Discount curve or discount curve handle.
#' @param fixings Optional fixing tibble.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with cashflows, discount factors, fixings, forwards, and PVs.
#'
#' @export
value_swap_cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle,
    discount_curve,
    fixings = NULL,
    evaluation_date = NULL
) {
  use_quantlib_GQH()

  cashflows <- swap_cashflow_schedule_from_trade_GQL(
    trade = trade,
    forecast_handle = forecast_handle
  )

  value_cashflow_schedule_GQL(
    cashflows = cashflows,
    discount_curve = discount_curve,
    forecast_curve = forecast_handle,
    fixings = fixings,
    evaluation_date = evaluation_date
  )
}

# R/swap_analytics.R

trade_product_GQL <- function(trade) {
  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  if (!("product" %in% names(trade))) {
    stop("trade must contain product column.")
  }

  product <- toupper(as.character(trade$product[[1]]))

  if (product %in% c("OIS", "EONIA_OIS", "OVERNIGHT_INDEXED_SWAP")) {
    return("OIS")
  }

  if (product %in% c("VANILLASWAP", "VANILLA_SWAP", "IRS", "INTEREST_RATE_SWAP")) {
    return("VANILLASWAP")
  }

  stop(
    "Unsupported product: ",
    product,
    ". Currently supported: OIS, VanillaSwap."
  )
}


#' Build cashflow schedule from one trade
#'
#' @param trade A one-row trade data frame.
#' @param forecast_handle Forecast curve handle.
#'
#' @return A tibble containing cashflows.
#'
#' @export
cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle
) {
  use_quantlib_GQH()

  product <- trade_product_GQL(trade)

  if (product == "OIS") {
    return(
      ois_cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle
      )
    )
  }

  if (product == "VANILLASWAP") {
    return(
      swap_cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle
      )
    )
  }

  stop("Unsupported product dispatch.")
}


#' Value cashflow schedule from one trade
#'
#' @param trade A one-row trade data frame.
#' @param forecast_handle Forecast curve handle.
#' @param discount_curve Discount curve or discount curve handle.
#' @param fixings Optional fixing tibble.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with cashflows, discount factors, fixings, forwards, and PVs.
#'
#' @export
value_cashflow_schedule_from_trade_GQL <- function(
    trade,
    forecast_handle,
    discount_curve,
    fixings = NULL,
    evaluation_date = NULL
) {
  use_quantlib_GQH()

  product <- trade_product_GQL(trade)

  if (product == "OIS") {
    return(
      value_ois_cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle,
        discount_curve = discount_curve,
        fixings = fixings,
        evaluation_date = evaluation_date
      )
    )
  }

  if (product == "VANILLASWAP") {
    return(
      value_swap_cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle,
        discount_curve = discount_curve,
        fixings = fixings,
        evaluation_date = evaluation_date
      )
    )
  }

  stop("Unsupported product dispatch.")
}


#' Build cashflow schedules from multiple trades
#'
#' @param trades A trade data frame.
#' @param forecast_handle Forecast curve handle.
#'
#' @return A tibble containing cashflows for all supported trades.
#'
#' @export
cashflow_schedule_from_trades_GQL <- function(
    trades,
    forecast_handle
) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  rows <- qlg_trade_rows(trades)

  out <- lapply(
    seq_along(rows),
    function(i) {
      trade <- rows[[i]]

      cf <- cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle
      )

      cf |>
        dplyr::mutate(
          trade_row = i,
          product = trade_product_GQL(trade)
        ) |>
        dplyr::relocate(.data$trade_row, .data$product)
    }
  )

  dplyr::bind_rows(out)
}


#' Value cashflow schedules from multiple trades
#'
#' @param trades A trade data frame.
#' @param forecast_handle Forecast curve handle.
#' @param discount_curve Discount curve or discount curve handle.
#' @param fixings Optional fixing tibble.
#' @param evaluation_date Evaluation date.
#'
#' @return A tibble with cashflows, discount factors, fixings, forwards, and PVs.
#'
#' @export
value_cashflow_schedule_from_trades_GQL <- function(
    trades,
    forecast_handle,
    discount_curve,
    fixings = NULL,
    evaluation_date = NULL
) {
  use_quantlib_GQH()
  requireNamespace("dplyr", quietly = TRUE)

  rows <- qlg_trade_rows(trades)

  out <- lapply(
    seq_along(rows),
    function(i) {
      trade <- rows[[i]]

      pv <- value_cashflow_schedule_from_trade_GQL(
        trade = trade,
        forecast_handle = forecast_handle,
        discount_curve = discount_curve,
        fixings = fixings,
        evaluation_date = evaluation_date
      )

      pv |>
        dplyr::mutate(
          trade_row = i,
          product = trade_product_GQL(trade)
        ) |>
        dplyr::relocate(.data$trade_row, .data$product)
    }
  )

  dplyr::bind_rows(out)
}
