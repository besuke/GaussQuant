# curve_nodes_GQL.R
#
# Currency-independent helpers for inspecting bootstrapped yield curves.

curve_dates_GQL <- function(curve) {
  curve_dates <- curve$dates()
  n_dates <- as.integer(curve_dates$size())

  purrr::map(
    seq_len(n_dates),
    function(i) curve_dates[i][[1L]]
  )
}

rate_helper_date_GQL <- function(
  helper,
  method = c("pillarDate", "latestDate")
) {
  method <- match.arg(method)

  tryCatch(
    switch(
      method,
      pillarDate = helper$pillarDate(),
      latestDate = helper$latestDate()
    ),
    error = function(e) NULL
  )
}

#' Extract curve nodes as forward rates safely
#'
#' Avoid direct use of `curve$nodes()`, which is unstable in some QuantLib
#' SWIG environments. Each node carries the continuously compounded forward
#' rate of the interval ending at that node; the first node repeats the first
#' interval rate.
#'
#' @param curve QuantLib yield curve object.
#'
#' @return A tibble containing node dates and continuously compounded forward
#'   rates.
#' @export
curve_forward_nodes_GQL <- function(curve) {
  dates_ql <- curve_dates_GQL(curve)
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
    node_date = as.Date(
      purrr::map_chr(
        dates_ql,
        safe_iso_GQH
      )
    ),
    node_rate = c(
      interval_rates[[1L]],
      interval_rates
    )
  )
}

#' Calculate a curve forward rate
#'
#' Setting `start_date` and `end_date` to the same date returns the
#' instantaneous forward rate implied by the curve.
#'
#' @param curve QuantLib yield curve object.
#' @param start_date Start date in ISO format.
#' @param end_date End date in ISO format.
#' @param day_counter QuantLib day-counter object.
#' @param compounding QuantLib compounding convention.
#'
#' @return A numeric forward rate.
#' @export
curve_forward_rate_GQL <- function(
  curve,
  start_date,
  end_date,
  day_counter,
  compounding = QuantLib::Compounding_Continuous_get()
) {
  tryCatch(
    curve$forwardRate(
      date_GQL(start_date),
      date_GQL(end_date),
      day_counter,
      compounding
    )$rate(),
    error = function(e) NA_real_
  )
}

#' Tabulate discount factors and zero rates at curve nodes
#'
#' @param curve QuantLib yield curve object.
#' @param day_counter QuantLib day counter for the zero rates. Defaults to the
#'   curve day counter.
#'
#' @return A tibble with ISO node dates, discount factors, and continuously
#'   compounded zero rates.
#' @export
curve_node_tbl_GQL <- function(curve, day_counter = NULL) {
  if (is.null(day_counter)) {
    day_counter <- curve$dayCounter()
  }

  dates_ql <- curve_dates_GQL(curve)

  tibble::tibble(
    date = purrr::map_chr(dates_ql, iso_GQL),
    discount_factor = purrr::map_dbl(
      dates_ql,
      function(date_ql) as.numeric(curve$discount(date_ql))
    ),
    zero_rate = purrr::map_dbl(
      dates_ql,
      function(date_ql) {
        QuantLib::InterestRate_rate(
          curve$zeroRate(date_ql, day_counter, "Continuous")
        )
      }
    )
  )
}
