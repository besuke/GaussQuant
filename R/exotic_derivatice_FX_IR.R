# exotic_derivatice_FX_IR.R
#
# Reusable FX / interest-rate exotic derivative helpers migrated from
# LagrangeFinance Chapter 15. The chapter should retain examples, tables,
# plots, and explanatory text; this file owns the calculation core.

.prdc_validate_matrix_GQL <- function(path_matrix) {
  path_matrix <- as.matrix(path_matrix)

  if (!is.numeric(path_matrix) || nrow(path_matrix) < 1L || ncol(path_matrix) < 1L) {
    stop("path_matrix must be a non-empty numeric matrix.", call. = FALSE)
  }

  path_matrix
}

.prdc_validate_coupon_terms_GQL <- function(
    fx_base,
    leverage,
    subtraction,
    floor_rate,
    cap_rate
) {
  finite_values <- c(
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction
  )

  if (any(!is.finite(finite_values))) {
    stop(
      "fx_base, leverage, and subtraction must be finite numeric values.",
      call. = FALSE
    )
  }

  if (length(floor_rate) != 1L || is.na(floor_rate) ||
      length(cap_rate) != 1L || is.na(cap_rate)) {
    stop("floor_rate and cap_rate must be non-missing scalars.", call. = FALSE)
  }

  if (fx_base <= 0) {
    stop("fx_base must be positive.", call. = FALSE)
  }

  if (floor_rate > cap_rate) {
    stop("floor_rate must not exceed cap_rate.", call. = FALSE)
  }

  invisible(
    c(
      finite_values,
      floor_rate = floor_rate,
      cap_rate = cap_rate
    )
  )
}

#' Build a Garman-Kohlhagen FX process
#'
#' @param spot Numeric FX spot or QuantLib QuoteHandle.
#' @param foreign_curve_handle QuantLib foreign yield-curve handle.
#' @param domestic_curve_handle QuantLib domestic yield-curve handle.
#' @param volatility_handle QuantLib Black volatility handle.
#'
#' @return QuantLib GarmanKohlagenProcess.
#' @export
fx_process_GQL <- function(
    spot,
    foreign_curve_handle,
    domestic_curve_handle,
    volatility_handle
) {
  use_quantlib_GQH()
  requireNamespace("QuantLib", quietly = TRUE)

  spot_handle <- if (is.numeric(spot)) {
    quote_handle_GQL(as.numeric(spot)[[1L]])
  } else {
    spot
  }

  QuantLib::GarmanKohlagenProcess(
    spot_handle,
    foreign_curve_handle,
    domestic_curve_handle,
    volatility_handle
  )
}

#' Build a coupon-date FX path generator
#'
#' The generator uses the actual coupon-date time grid, so unequal time
#' intervals are preserved. Standard-normal draws are passed directly to
#' QuantLib's process evolve method.
#'
#' @param valuation_date ISO date, R Date, or QuantLib Date.
#' @param coupon_schedule QuantLib Schedule.
#' @param day_counter QuantLib day counter.
#' @param process QuantLib one-dimensional stochastic process.
#'
#' @return A list containing the remaining coupon dates, time grid, time
#'   steps, and a `next_path` function.
#' @export
fx_path_generator_GQL <- function(
    valuation_date,
    coupon_schedule,
    day_counter,
    process
) {
  valuation_date_ql <- .as_ql_date_GQL(valuation_date)
  valuation_date_r <- as_r_date_GQH(valuation_date_ql)
  coupon_dates_ql <- .schedule_date_vector_ql_GQL(coupon_schedule)

  remaining_index <- purrr::map_lgl(
    coupon_dates_ql,
    ~ as_r_date_GQH(.x) > valuation_date_r
  )

  remaining_coupon_dates_ql <- coupon_dates_ql[remaining_index]

  if (length(remaining_coupon_dates_ql) == 0L) {
    stop("coupon_schedule has no coupon date after valuation_date.", call. = FALSE)
  }

  coupon_times <- purrr::map_dbl(
    remaining_coupon_dates_ql,
    ~ as.numeric(day_counter$yearFraction(valuation_date_ql, .x))
  )

  if (any(!is.finite(coupon_times)) || any(diff(coupon_times) <= 0)) {
    stop("Coupon times must be finite and strictly increasing.", call. = FALSE)
  }

  time_grid <- c(0, coupon_times)
  time_steps <- diff(time_grid)
  number_of_steps <- length(time_steps)

  next_path <- function() {
    normal_draws <- stats::rnorm(number_of_steps)

    path_states <- purrr::accumulate(
      seq_len(number_of_steps),
      .init = as.numeric(process$x0()),
      .f = function(spot_value, step_id) {
        as.numeric(
          process$evolve(
            time_grid[[step_id]],
            spot_value,
            time_steps[[step_id]],
            normal_draws[[step_id]]
          )
        )
      }
    )

    as.numeric(path_states[-1L])
  }

  list(
    valuation_date = valuation_date_r,
    remaining_coupon_dates = as.Date(
      purrr::map_chr(remaining_coupon_dates_ql, iso_GQL)
    ),
    remaining_coupon_dates_ql = remaining_coupon_dates_ql,
    coupon_times = coupon_times,
    time_grid = time_grid,
    time_steps = time_steps,
    number_of_steps = number_of_steps,
    next_path = next_path
  )
}

#' Generate a PRDC FX path matrix
#'
#' @param path_generator Object returned by `fx_path_generator_GQL()`.
#' @param n_paths Number of Monte Carlo paths.
#' @param seed Random seed.
#'
#' @return Numeric matrix with coupon dates in rows and paths in columns.
#' @export
prdc_path_matrix_GQL <- function(
    path_generator,
    n_paths,
    seed = 1L
) {
  n_paths <- as.integer(n_paths)

  if (length(n_paths) != 1L || is.na(n_paths) || n_paths < 1L) {
    stop("n_paths must be a positive integer.", call. = FALSE)
  }

  set.seed(as.integer(seed))

  paths <- purrr::map(
    seq_len(n_paths),
    ~ path_generator$next_path()
  )

  path_matrix <- do.call(cbind, paths)
  storage.mode(path_matrix) <- "double"
  path_matrix
}

#' Calculate a PRDC coupon rate
#'
#' @param fx_rate Numeric FX rate vector or matrix.
#' @param fx_base Base FX rate in the coupon formula.
#' @param leverage FX leverage coefficient.
#' @param subtraction Fixed subtraction rate.
#' @param floor_rate Coupon floor.
#' @param cap_rate Coupon cap.
#'
#' @return Coupon rates with the same dimensions as `fx_rate`.
#' @export
prdc_coupon_GQL <- function(
    fx_rate,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf
) {
  .prdc_validate_coupon_terms_GQL(
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction,
    floor_rate = floor_rate,
    cap_rate = cap_rate
  )

  uncapped_coupon <- as.numeric(leverage) *
    (fx_rate / as.numeric(fx_base)) -
    as.numeric(subtraction)

  pmin(
    pmax(uncapped_coupon, as.numeric(floor_rate)),
    as.numeric(cap_rate)
  )
}

#' Summarise expected PRDC coupons
#'
#' @param path_matrix FX path matrix with coupon dates in rows.
#' @param coupon_dates Coupon dates corresponding to matrix rows.
#' @param coupon_times Coupon times corresponding to matrix rows.
#' @inheritParams prdc_coupon_GQL
#' @param intro_coupon_dates Optional dates receiving a fixed introductory rate.
#' @param intro_coupon_rate Optional fixed introductory coupon rate.
#'
#' @return Tibble of expected FX and coupon statistics.
#' @export
prdc_expected_coupon_GQL <- function(
    path_matrix,
    coupon_dates,
    coupon_times,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    intro_coupon_dates = NULL,
    intro_coupon_rate = NULL
) {
  path_matrix <- .prdc_validate_matrix_GQL(path_matrix)
  coupon_dates <- as.Date(coupon_dates)
  coupon_times <- as.numeric(coupon_times)
  number_of_steps <- nrow(path_matrix)

  if (length(coupon_dates) != number_of_steps ||
      length(coupon_times) != number_of_steps) {
    stop(
      "coupon_dates and coupon_times must match the path-matrix row count.",
      call. = FALSE
    )
  }

  coupon_path_matrix <- prdc_coupon_GQL(
    fx_rate = path_matrix,
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction,
    floor_rate = floor_rate,
    cap_rate = cap_rate
  )

  is_intro_coupon <- if (is.null(intro_coupon_dates)) {
    rep(FALSE, number_of_steps)
  } else {
    coupon_dates %in% as.Date(intro_coupon_dates)
  }

  expected_coupon_rate <- rowMeans(coupon_path_matrix)
  final_coupon_rate <- if (is.null(intro_coupon_rate)) {
    expected_coupon_rate
  } else {
    dplyr::if_else(
      is_intro_coupon,
      as.numeric(intro_coupon_rate),
      expected_coupon_rate
    )
  }

  tibble::tibble(
    coupon_number = seq_len(number_of_steps),
    coupon_date = coupon_dates,
    time = coupon_times,
    expected_fx = rowMeans(path_matrix),
    expected_coupon_rate = expected_coupon_rate,
    fx_standard_deviation = purrr::map_dbl(
      seq_len(number_of_steps),
      ~ stats::sd(path_matrix[.x, ])
    ),
    coupon_standard_deviation = purrr::map_dbl(
      seq_len(number_of_steps),
      ~ stats::sd(coupon_path_matrix[.x, ])
    ),
    is_intro_coupon = is_intro_coupon,
    final_coupon_rate = final_coupon_rate
  )
}

.prdc_coupon_period_table_GQL <- function(
    valuation_date,
    coupon_schedule,
    day_counter
) {
  valuation_date_ql <- .as_ql_date_GQL(valuation_date)
  valuation_date_r <- as_r_date_GQH(valuation_date_ql)
  schedule_dates_ql <- .schedule_date_vector_ql_GQL(coupon_schedule)

  if (length(schedule_dates_ql) < 2L) {
    stop("coupon_schedule must contain at least two dates.", call. = FALSE)
  }

  tibble::tibble(
    accrual_start_ql = schedule_dates_ql[-length(schedule_dates_ql)],
    accrual_end_ql = schedule_dates_ql[-1L]
  ) |>
    dplyr::mutate(
      accrual_start = as.Date(
        purrr::map_chr(.data$accrual_start_ql, iso_GQL)
      ),
      accrual_end = as.Date(
        purrr::map_chr(.data$accrual_end_ql, iso_GQL)
      ),
      accrual_fraction = purrr::map2_dbl(
        .data$accrual_start_ql,
        .data$accrual_end_ql,
        ~ as.numeric(day_counter$yearFraction(.x, .y))
      )
    ) |>
    dplyr::filter(.data$accrual_end > valuation_date_r) |>
    dplyr::mutate(coupon_number = dplyr::row_number()) |>
    dplyr::select(
      "coupon_number",
      "accrual_start_ql",
      "accrual_end_ql",
      "accrual_start",
      "accrual_end",
      "accrual_fraction"
    )
}

#' Build a PRDC expected cash-flow table
#'
#' @param expected_coupon_tbl Output from `prdc_expected_coupon_GQL()`.
#' @param valuation_date ISO date, R Date, or QuantLib Date.
#' @param coupon_schedule QuantLib Schedule.
#' @param day_counter QuantLib day counter.
#' @param discount_curve QuantLib discount curve or handle.
#' @param notional Product notional.
#' @param redemption_amount Redemption amount paid on the final coupon date.
#'
#' @return Tibble containing coupon, redemption, and present values.
#' @export
prdc_cashflow_table_GQL <- function(
    expected_coupon_tbl,
    valuation_date,
    coupon_schedule,
    day_counter,
    discount_curve,
    notional,
    redemption_amount = notional
) {
  required_columns <- c(
    "coupon_number",
    "coupon_date",
    "final_coupon_rate"
  )

  if (!all(required_columns %in% names(expected_coupon_tbl))) {
    stop(
      "expected_coupon_tbl must contain: ",
      paste(required_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  period_tbl <- .prdc_coupon_period_table_GQL(
    valuation_date = valuation_date,
    coupon_schedule = coupon_schedule,
    day_counter = day_counter
  )

  if (nrow(period_tbl) != nrow(expected_coupon_tbl)) {
    stop(
      "Expected-coupon rows do not match the remaining coupon periods.",
      call. = FALSE
    )
  }

  period_tbl |>
    dplyr::left_join(
      expected_coupon_tbl,
      by = "coupon_number"
    ) |>
    dplyr::mutate(
      coupon_amount = as.numeric(notional) *
        .data$final_coupon_rate *
        .data$accrual_fraction,
      discount_factor = purrr::map_dbl(
        .data$accrual_end_ql,
        ~ curve_discount_safe_GQL(discount_curve, .x)
      ),
      coupon_present_value = .data$coupon_amount *
        .data$discount_factor,
      redemption_amount = dplyr::if_else(
        .data$coupon_number == max(.data$coupon_number),
        as.numeric(redemption_amount),
        0
      ),
      redemption_present_value = .data$redemption_amount *
        .data$discount_factor,
      total_amount = .data$coupon_amount + .data$redemption_amount,
      total_present_value = .data$coupon_present_value +
        .data$redemption_present_value
    ) |>
    dplyr::select(
      -dplyr::any_of(c("accrual_start_ql", "accrual_end_ql"))
    )
}

#' Value a non-callable PRDC by Monte Carlo
#'
#' @param valuation_date ISO date, R Date, or QuantLib Date.
#' @param coupon_schedule QuantLib Schedule.
#' @param process QuantLib FX stochastic process.
#' @param discount_curve QuantLib discount curve or handle.
#' @param notional Product notional.
#' @inheritParams prdc_coupon_GQL
#' @param day_counter QuantLib day counter.
#' @param intro_coupon_dates Optional dates receiving a fixed introductory rate.
#' @param intro_coupon_rate Optional fixed introductory coupon rate.
#' @param n_paths Number of Monte Carlo paths.
#' @param seed Random seed.
#' @param redemption_amount Redemption amount paid at maturity.
#' @param return_details Return all intermediate results instead of a numeric NPV.
#'
#' @return Numeric NPV, or a detailed valuation list.
#' @export
prdc_npv_GQL <- function(
    valuation_date,
    coupon_schedule,
    process,
    discount_curve,
    notional,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    day_counter = QuantLib::Actual360(),
    intro_coupon_dates = NULL,
    intro_coupon_rate = NULL,
    n_paths = 10000L,
    seed = 1L,
    redemption_amount = notional,
    return_details = FALSE
) {
  set_eval_date_GQL(valuation_date)

  path_generator <- fx_path_generator_GQL(
    valuation_date = valuation_date,
    coupon_schedule = coupon_schedule,
    day_counter = day_counter,
    process = process
  )

  path_matrix <- prdc_path_matrix_GQL(
    path_generator = path_generator,
    n_paths = n_paths,
    seed = seed
  )

  expected_coupon_tbl <- prdc_expected_coupon_GQL(
    path_matrix = path_matrix,
    coupon_dates = path_generator$remaining_coupon_dates,
    coupon_times = path_generator$coupon_times,
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction,
    floor_rate = floor_rate,
    cap_rate = cap_rate,
    intro_coupon_dates = intro_coupon_dates,
    intro_coupon_rate = intro_coupon_rate
  )

  cashflow_tbl <- prdc_cashflow_table_GQL(
    expected_coupon_tbl = expected_coupon_tbl,
    valuation_date = valuation_date,
    coupon_schedule = coupon_schedule,
    day_counter = day_counter,
    discount_curve = discount_curve,
    notional = notional,
    redemption_amount = redemption_amount
  )

  coupon_leg_npv <- sum(cashflow_tbl$coupon_present_value)
  redemption_leg_npv <- sum(cashflow_tbl$redemption_present_value)
  total_npv <- coupon_leg_npv + redemption_leg_npv

  if (!isTRUE(return_details)) {
    return(total_npv)
  }

  list(
    npv = total_npv,
    summary = tibble::tribble(
      ~metric, ~value,
      "coupon_leg_npv", coupon_leg_npv,
      "redemption_leg_npv", redemption_leg_npv,
      "total_npv", total_npv,
      "npv_as_percent_of_notional", total_npv / as.numeric(notional),
      "number_of_paths", as.numeric(n_paths)
    ),
    path_generator = path_generator,
    path_matrix = path_matrix,
    expected_coupon = expected_coupon_tbl,
    cashflows = cashflow_tbl
  )
}

#' Calculate PRDC coupon floor and cap probabilities
#'
#' @param path_matrix FX path matrix.
#' @param coupon_dates Optional coupon dates.
#' @param coupon_times Optional coupon times.
#' @inheritParams prdc_coupon_GQL
#' @param tolerance Numerical comparison tolerance.
#'
#' @return Tibble of floor, cap, and interior probabilities.
#' @export
prdc_cap_floor_probability_GQL <- function(
    path_matrix,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    coupon_dates = NULL,
    coupon_times = NULL,
    tolerance = 1e-12
) {
  path_matrix <- .prdc_validate_matrix_GQL(path_matrix)
  number_of_steps <- nrow(path_matrix)

  coupon_matrix <- prdc_coupon_GQL(
    fx_rate = path_matrix,
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction,
    floor_rate = floor_rate,
    cap_rate = cap_rate
  )

  result <- tibble::tibble(
    coupon_number = seq_len(number_of_steps),
    floor_probability = rowMeans(
      coupon_matrix <= as.numeric(floor_rate) + tolerance
    ),
    cap_probability = rowMeans(
      coupon_matrix >= as.numeric(cap_rate) - tolerance
    )
  ) |>
    dplyr::mutate(
      interior_probability = 1 -
        .data$floor_probability -
        .data$cap_probability
    )

  if (!is.null(coupon_dates)) {
    result$coupon_date <- as.Date(coupon_dates)
  }

  if (!is.null(coupon_times)) {
    result$time <- as.numeric(coupon_times)
  }

  result
}

#' Estimate PRDC Monte Carlo error
#'
#' @param path_matrix FX path matrix.
#' @param cashflow_tbl Output from `prdc_cashflow_table_GQL()`.
#' @param notional Product notional.
#' @inheritParams prdc_coupon_GQL
#' @param intro_coupon_dates Optional fixed-coupon dates.
#' @param intro_coupon_rate Optional fixed introductory coupon rate.
#' @param confidence_level Confidence level for the normal interval.
#'
#' @return List containing path present values and an error summary.
#' @export
prdc_monte_carlo_error_GQL <- function(
    path_matrix,
    cashflow_tbl,
    notional,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    intro_coupon_dates = NULL,
    intro_coupon_rate = NULL,
    confidence_level = 0.95
) {
  path_matrix <- .prdc_validate_matrix_GQL(path_matrix)

  required_columns <- c(
    "coupon_date",
    "accrual_fraction",
    "discount_factor",
    "redemption_present_value"
  )

  if (!all(required_columns %in% names(cashflow_tbl))) {
    stop(
      "cashflow_tbl must contain: ",
      paste(required_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (nrow(path_matrix) != nrow(cashflow_tbl)) {
    stop("path_matrix rows must match cashflow_tbl rows.", call. = FALSE)
  }

  coupon_matrix <- prdc_coupon_GQL(
    fx_rate = path_matrix,
    fx_base = fx_base,
    leverage = leverage,
    subtraction = subtraction,
    floor_rate = floor_rate,
    cap_rate = cap_rate
  )

  if (!is.null(intro_coupon_dates) && !is.null(intro_coupon_rate)) {
    intro_index <- as.Date(cashflow_tbl$coupon_date) %in%
      as.Date(intro_coupon_dates)

    coupon_matrix[intro_index, ] <- as.numeric(intro_coupon_rate)
  }

  coupon_weights <- as.numeric(notional) *
    cashflow_tbl$accrual_fraction *
    cashflow_tbl$discount_factor

  coupon_leg_pv <- colSums(coupon_matrix * coupon_weights)
  redemption_leg_pv <- sum(cashflow_tbl$redemption_present_value)
  total_pv <- coupon_leg_pv + redemption_leg_pv
  number_of_paths <- length(total_pv)
  standard_error <- stats::sd(total_pv) / sqrt(number_of_paths)
  alpha <- 1 - as.numeric(confidence_level)
  critical_value <- stats::qnorm(1 - alpha / 2)
  mean_present_value <- mean(total_pv)

  list(
    path_present_values = tibble::tibble(
      path_id = seq_len(number_of_paths),
      coupon_leg_pv = coupon_leg_pv,
      redemption_leg_pv = redemption_leg_pv,
      total_pv = total_pv
    ),
    summary = tibble::tribble(
      ~metric, ~value,
      "number_of_paths", number_of_paths,
      "mean_present_value", mean_present_value,
      "present_value_sd", stats::sd(total_pv),
      "standard_error", standard_error,
      "lower_confidence_bound",
      mean_present_value - critical_value * standard_error,
      "upper_confidence_bound",
      mean_present_value + critical_value * standard_error,
      "confidence_level", confidence_level
    )
  )
}

#' Calculate PRDC FX spot sensitivity
#'
#' @param spot_scenarios Data frame with `scenario` and `fx_spot`.
#' @param foreign_curve_handle QuantLib foreign yield-curve handle.
#' @param domestic_curve_handle QuantLib domestic yield-curve handle.
#' @param volatility_handle QuantLib Black volatility handle.
#' @param base_scenario Scenario used for the difference column.
#' @inheritParams prdc_npv_GQL
#'
#' @return Scenario tibble with NPV differences.
#' @export
prdc_spot_sensitivity_GQL <- function(
    spot_scenarios,
    valuation_date,
    coupon_schedule,
    foreign_curve_handle,
    domestic_curve_handle,
    volatility_handle,
    discount_curve,
    notional,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    day_counter = QuantLib::Actual360(),
    intro_coupon_dates = NULL,
    intro_coupon_rate = NULL,
    n_paths = 2000L,
    seed = 1L,
    redemption_amount = notional,
    base_scenario = "base"
) {
  if (!all(c("scenario", "fx_spot") %in% names(spot_scenarios))) {
    stop(
      "spot_scenarios must contain scenario and fx_spot columns.",
      call. = FALSE
    )
  }

  result <- spot_scenarios |>
    dplyr::mutate(
      prdc_npv = purrr::map_dbl(
        .data$fx_spot,
        function(spot_value) {
          process <- fx_process_GQL(
            spot = spot_value,
            foreign_curve_handle = foreign_curve_handle,
            domestic_curve_handle = domestic_curve_handle,
            volatility_handle = volatility_handle
          )

          prdc_npv_GQL(
            valuation_date = valuation_date,
            coupon_schedule = coupon_schedule,
            process = process,
            discount_curve = discount_curve,
            notional = notional,
            fx_base = fx_base,
            leverage = leverage,
            subtraction = subtraction,
            floor_rate = floor_rate,
            cap_rate = cap_rate,
            day_counter = day_counter,
            intro_coupon_dates = intro_coupon_dates,
            intro_coupon_rate = intro_coupon_rate,
            n_paths = n_paths,
            seed = seed,
            redemption_amount = redemption_amount
          )
        }
      )
    )

  base_index <- match(base_scenario, result$scenario)

  if (is.na(base_index)) {
    stop("base_scenario was not found in spot_scenarios.", call. = FALSE)
  }

  result |>
    dplyr::mutate(
      npv_difference_from_base = .data$prdc_npv -
        .data$prdc_npv[[base_index]]
    )
}

#' Calculate PRDC FX volatility sensitivity
#'
#' @param volatility_scenarios Data frame with `scenario` and `fx_volatility`.
#' @param spot Numeric FX spot or QuantLib QuoteHandle.
#' @param foreign_curve_handle QuantLib foreign yield-curve handle.
#' @param domestic_curve_handle QuantLib domestic yield-curve handle.
#' @param calendar QuantLib calendar for the volatility curve.
#' @param base_scenario Scenario used for the difference column.
#' @inheritParams prdc_npv_GQL
#'
#' @return Scenario tibble with NPV differences.
#' @export
prdc_volatility_sensitivity_GQL <- function(
    volatility_scenarios,
    valuation_date,
    coupon_schedule,
    spot,
    foreign_curve_handle,
    domestic_curve_handle,
    discount_curve,
    notional,
    fx_base,
    leverage,
    subtraction,
    floor_rate = 0,
    cap_rate = Inf,
    day_counter = QuantLib::Actual360(),
    calendar = QuantLib::NullCalendar(),
    intro_coupon_dates = NULL,
    intro_coupon_rate = NULL,
    n_paths = 2000L,
    seed = 1L,
    redemption_amount = notional,
    base_scenario = "base"
) {
  if (!all(c("scenario", "fx_volatility") %in% names(volatility_scenarios))) {
    stop(
      "volatility_scenarios must contain scenario and fx_volatility columns.",
      call. = FALSE
    )
  }

  valuation_date_ql <- .as_ql_date_GQL(valuation_date)

  result <- volatility_scenarios |>
    dplyr::mutate(
      prdc_npv = purrr::map_dbl(
        .data$fx_volatility,
        function(volatility_value) {
          volatility_curve <- QuantLib::BlackConstantVol(
            valuation_date_ql,
            calendar,
            quote_handle_GQL(volatility_value),
            day_counter
          )

          volatility_handle <- QuantLib::BlackVolTermStructureHandle(
            volatility_curve
          )

          process <- fx_process_GQL(
            spot = spot,
            foreign_curve_handle = foreign_curve_handle,
            domestic_curve_handle = domestic_curve_handle,
            volatility_handle = volatility_handle
          )

          prdc_npv_GQL(
            valuation_date = valuation_date,
            coupon_schedule = coupon_schedule,
            process = process,
            discount_curve = discount_curve,
            notional = notional,
            fx_base = fx_base,
            leverage = leverage,
            subtraction = subtraction,
            floor_rate = floor_rate,
            cap_rate = cap_rate,
            day_counter = day_counter,
            intro_coupon_dates = intro_coupon_dates,
            intro_coupon_rate = intro_coupon_rate,
            n_paths = n_paths,
            seed = seed,
            redemption_amount = redemption_amount
          )
        }
      )
    )

  base_index <- match(base_scenario, result$scenario)

  if (is.na(base_index)) {
    stop("base_scenario was not found in volatility_scenarios.", call. = FALSE)
  }

  result |>
    dplyr::mutate(
      npv_difference_from_base = .data$prdc_npv -
        .data$prdc_npv[[base_index]]
    )
}
