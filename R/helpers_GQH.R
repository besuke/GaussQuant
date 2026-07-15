# lagrange_helpers.R
#
# Transitional helpers migrated from LagrangeFinance chapters 7-14.
# These functions intentionally collect reusable QuantLib and numerical
# operations in one place so LagrangeFinance can focus on examples,
# tables, plots, and explanatory text.

#' Convert a metric value to character
#'
#' @param x Value or list-column element.
#' @return Character scalar.
#' @export
metric_value_to_chr_GQH <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }

  x1 <- if (is.list(x)) x[[1L]] else x[[1L]]

  if (length(x1) == 0L || all(is.na(x1))) {
    return(NA_character_)
  }

  if (inherits(x1, "Date")) {
    return(as.character(x1))
  }

  if (is.numeric(x1)) {
    return(sprintf("%.8f", as.numeric(x1)))
  }

  as.character(x1)
}


#' Format a metric table for display
#'
#' @param tbl Metric table.
#' @return A tibble with a character value column when present.
#' @export
metric_tbl_display_GQH <- function(tbl) {
  if (!"value" %in% names(tbl)) {
    return(tbl)
  }

  tbl |>
    dplyr::mutate(
      value = purrr::map_chr(.data$value, metric_value_to_chr_GQH)
    )
}


#' Extract a numeric metric value
#'
#' @param tbl Metric table.
#' @param metric_name Metric name.
#' @return Numeric scalar or NA.
#' @export
get_metric_num_GQH <- function(tbl, metric_name) {
  if (!all(c("metric", "value") %in% names(tbl))) {
    return(NA_real_)
  }

  idx <- match(metric_name, tbl$metric)

  if (is.na(idx)) {
    return(NA_real_)
  }

  x <- tbl$value[[idx]]

  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  x1 <- if (is.list(x)) x[[1L]] else x[[1L]]
  suppressWarnings(as.numeric(x1))
}


#' Replace or append a metric value
#'
#' @param tbl Metric table.
#' @param metric_name Metric name.
#' @param new_value Replacement value.
#' @return Updated metric table.
#' @export
replace_metric_value_GQH <- function(tbl, metric_name, new_value) {
  tbl <- tbl |>
    dplyr::mutate(value = as.list(.data$value))

  idx <- match(metric_name, tbl$metric)

  if (is.na(idx)) {
    return(
      dplyr::bind_rows(
        tbl,
        tibble::tibble(
          metric = metric_name,
          value = list(new_value)
        )
      )
    )
  }

  tbl$value[idx] <- list(new_value)
  tbl
}


#' Safely convert a value to numeric
#'
#' @param x Object convertible to numeric.
#' @return Numeric scalar or NA.
#' @export
safe_num_GQH <- function(x) {
  tryCatch(
    as.numeric(x),
    error = function(e) NA_real_
  )
}


#' Safely convert a QuantLib date to ISO
#'
#' @param x QuantLib Date object or NULL.
#' @return ISO date string or NA.
#' @export
safe_iso_GQH <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  tryCatch(
    iso_GQL(x),
    error = function(e) NA_character_
  )
}


.schedule_date_vector_ql_GQL <- function(schedule) {
  purrr::map(
    seq_len(schedule$size()),
    function(i) {
      date_ql <- .schedule_date_at_ql_GQL(
        schedule = schedule,
        i_one_based = i
      )

      if (is.null(date_ql)) {
        stop(
          "Unable to access schedule date at index ",
          i,
          call. = FALSE
        )
      }

      date_ql
    }
  )
}

#' Build an R Date vector from a QuantLib schedule
#'
#' @param schedule QuantLib Schedule object.
#' @return An R Date vector.
#' @export
schedule_date_vector_GQL <- function(schedule) {
  as.Date(
    purrr::map_chr(
      .schedule_date_vector_ql_GQL(schedule),
      iso_GQL
    )
  )
}

#' Build a schedule table with period numbers
#'
#' @param schedule QuantLib Schedule object.
#' @return Tibble with period and schedule_date.
#' @export
schedule_table_GQL <- function(schedule) {
  dates <- schedule_date_vector_GQL(schedule)

  tibble::tibble(
    period = seq_along(dates),
    schedule_date = dates
  )
}

#' Safely obtain a discount factor
#'
#' @param curve QuantLib yield curve.
#' @param x QuantLib Date or time.
#' @return Numeric discount factor or NA.
#' @export
curve_discount_safe_GQL <- function(curve, x) {
  tryCatch(
    curve$discount(x),
    error = function(e) NA_real_
  )
}


#' Build a dense curve grid table
#'
#' @param curve QuantLib yield term structure.
#' @param n Number of grid points.
#' @param extrapolate Enable extrapolation.
#' @return Tibble with time, date, discount factor, and zero rate.
#' @export
curve_grid_tbl_GQL <- function(curve, n = 200L, extrapolate = TRUE) {
  if (isTRUE(extrapolate)) {
    tryCatch(
      QuantLib::TermStructure_enableExtrapolation(curve),
      error = function(e) NULL
    )
  }

  reference_date <- as.Date(iso_GQL(curve$referenceDate()))
  max_time <- as.numeric(curve$maxTime())
  times <- seq(0, max_time, length.out = as.integer(n))

  tibble::tibble(time = times) |>
    dplyr::mutate(
      discount_factor = purrr::map_dbl(
        .data$time,
        ~ curve_discount_safe_GQL(curve, .x)
      ),
      zero_rate = dplyr::if_else(
        .data$time > 0 & .data$discount_factor > 0,
        -log(.data$discount_factor) / .data$time,
        0
      ),
      curve_date = reference_date + round(.data$time * 365)
    )
}


#' Build a flat yield curve
#'
#' @param reference_date QuantLib Date.
#' @param rate Flat rate.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @return QuantLib FlatForward object.
#' @export
flat_curve_GQL <- function(
    reference_date,
    rate,
    day_counter = QuantLib::Actual365Fixed(),
    compounding = "Continuous"
) {
  QuantLib::FlatForward(
    reference_date,
    rate,
    day_counter,
    compounding
  )
}


#' Build a zero-spreaded yield curve
#'
#' @param base_curve_handle QuantLib yield curve handle.
#' @param spread_rate Zero-rate spread.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention.
#' @param frequency QuantLib frequency convention.
#' @param extrapolate Enable extrapolation.
#' @return List containing curve, curve_handle, and spread_handle.
#' @export
make_zero_spreaded_curve_GQL <- function(
    base_curve_handle,
    spread_rate,
    day_counter = QuantLib::Actual365Fixed(),
    compounding = QuantLib::Compounding_Simple_get(),
    frequency = QuantLib::Frequency_Annual_get(),
    extrapolate = TRUE
) {
  spread_handle <- quote_handle_GQL(spread_rate)

  curve <- QuantLib::ZeroSpreadedTermStructure(
    base_curve_handle,
    spread_handle,
    compounding,
    frequency,
    day_counter
  )

  if (isTRUE(extrapolate)) {
    tryCatch(
      QuantLib::TermStructure_enableExtrapolation(curve),
      error = function(e) NULL
    )
  }

  list(
    curve = curve,
    curve_handle = QuantLib::YieldTermStructureHandle(curve),
    spread_handle = spread_handle
  )
}


#' Push calibration helpers into a QuantLib vector
#'
#' @param helpers List of QuantLib calibration helpers.
#' @return QuantLib CalibrationHelperVector.
#' @export
push_calibration_helpers_GQL <- function(helpers) {
  vec <- QuantLib::CalibrationHelperVector()

  purrr::walk(
    helpers,
    function(helper) {
      pushed <- tryCatch(
        {
          QuantLib::CalibrationHelperVector_push_back(vec, helper)
          TRUE
        },
        error = function(e) FALSE
      )

      if (!pushed) {
        QuantLib::CalibrationHelperVector_append(vec, helper)
      }
    }
  )

  vec
}


#' Complete bond accrual metrics from a schedule
#'
#' @param price_tbl Bond price metric table.
#' @param bond_schedule QuantLib Schedule.
#' @param settlement_date Settlement date.
#' @param day_counter QuantLib day counter.
#' @return Updated metric table.
#' @export
complete_accrual_metrics_GQL <- function(
    price_tbl,
    bond_schedule,
    settlement_date,
    day_counter
) {
  settlement_date_ql <- if (is.character(settlement_date) || inherits(settlement_date, "Date")) {
    date_GQL(settlement_date)
  } else {
    settlement_date
  }

  settlement_date_chr <- iso_GQL(settlement_date_ql)

  schedule_dates_chr <- schedule_dates_GQL(bond_schedule)[[1L]] |>
    as.character()

  schedule_dates_chr <- schedule_dates_chr[
    !is.na(schedule_dates_chr) &
      schedule_dates_chr <= settlement_date_chr
  ]

  previous_coupon_date_chr <- if (length(schedule_dates_chr) == 0L) {
    NA_character_
  } else {
    max(schedule_dates_chr)
  }

  if (is.na(previous_coupon_date_chr)) {
    return(price_tbl)
  }

  previous_coupon_date <- date_GQL(previous_coupon_date_chr)

  accrued_days <- tryCatch(
    day_counter$dayCount(
      previous_coupon_date,
      settlement_date_ql
    ),
    error = function(e) {
      as.integer(
        round(
          day_counter$yearFraction(
            previous_coupon_date,
            settlement_date_ql
          ) * 360
        )
      )
    }
  )

  price_tbl |>
    replace_metric_value_GQH(
      "previous_coupon_date",
      as.Date(previous_coupon_date_chr)
    ) |>
    replace_metric_value_GQH(
      "accrued_days",
      as.numeric(accrued_days)
    )
}


#' Safely calculate bond yield from clean price
#'
#' @param bond QuantLib bond.
#' @param clean_price Clean price.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Coupon frequency.
#' @return Numeric yield.
#' @export
bond_yield_from_clean_safe_GQL <- function(
    bond,
    clean_price,
    day_counter,
    compounding,
    frequency
) {
  attempts <- list(
    function() {
      bond_yield_from_clean_price_GQL(
        bond = bond,
        clean_price = clean_price,
        day_counter = day_counter,
        compounding = compounding,
        frequency = frequency
      )
    },
    function() {
      bond_yield_GQL(
        bond = bond,
        clean_price = clean_price,
        day_counter = day_counter,
        compounding = compounding,
        frequency = frequency
      )
    },
    function() {
      QuantLib::BondFunctions_yield(
        bond,
        clean_price,
        day_counter,
        compounding,
        frequency
      )
    }
  )

  result <- purrr::detect(
    purrr::map(
      attempts,
      ~ tryCatch(.x(), error = function(e) NULL)
    ),
    ~ !is.null(.x)
  )

  if (is.null(result)) {
    stop("Unable to calculate bond yield from clean price.", call. = FALSE)
  }

  as.numeric(result)
}


#' Safely calculate a bond clean price from yield
#'
#' @param bond QuantLib bond.
#' @param yield_rate Yield.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Coupon frequency.
#' @param settlement_date Optional settlement date.
#' @return Numeric clean price or NA.
#' @export
bond_clean_price_from_yield_safe_GQL <- function(
    bond,
    yield_rate,
    day_counter,
    compounding,
    frequency,
    settlement_date = NULL
) {
  if (is.null(settlement_date)) {
    first_result <- tryCatch(
      bond$cleanPrice(
        yield_rate,
        day_counter,
        compounding,
        frequency
      ),
      error = function(e) NULL
    )

    if (!is.null(first_result)) {
      return(as.numeric(first_result))
    }

    settlement_date <- tryCatch(
      bond$settlementDate(),
      error = function(e) NULL
    )
  }

  if (is.null(settlement_date)) {
    return(NA_real_)
  }

  tryCatch(
    as.numeric(
      bond$cleanPrice(
        yield_rate,
        day_counter,
        compounding,
        frequency,
        settlement_date
      )
    ),
    error = function(e) NA_real_
  )
}


#' Safely calculate an instrument NPV
#'
#' @param instrument QuantLib instrument.
#' @return Numeric NPV or NA.
#' @export
instrument_npv_safe_GQL <- function(instrument) {
  tryCatch(
    as.numeric(instrument$NPV()),
    error = function(e) NA_real_
  )
}


#' Calculate a Treasury futures CTD table
#'
#' @param deliverable_tbl Deliverable basket data.
#' @param settlement_date Settlement date.
#' @param futures_price Futures price.
#' @param repo_end_date Repo end date.
#' @param repo_rate Repo rate.
#' @param settlement_days Settlement days.
#' @param calendar QuantLib calendar.
#' @param day_counter Bond day counter.
#' @param compounding Compounding convention.
#' @param frequency Coupon frequency.
#' @param repo_day_counter Repo day counter.
#' @param carry_day_counter Carry day counter.
#' @return List with basket and CTD row.
#' @export
bond_futures_ctd_table_GQL <- function(
    deliverable_tbl,
    settlement_date,
    futures_price,
    repo_end_date,
    repo_rate,
    settlement_days = 1L,
    calendar = QuantLib::UnitedStates("GovernmentBond"),
    day_counter = QuantLib::ActualActual("Bond"),
    compounding = QuantLib::Compounding_Compounded_get(),
    frequency = QuantLib::Frequency_Semiannual_get(),
    repo_day_counter = QuantLib::Actual360(),
    carry_day_counter = QuantLib::Actual360()
) {
  required_columns <- c(
    "issue_date",
    "maturity_date",
    "coupon_rate_pct",
    "conversion_factor",
    "market_yield_pct"
  )

  missing_columns <- setdiff(required_columns, names(deliverable_tbl))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing deliverable columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  gross_tbl <- purrr::pmap_dfr(
    deliverable_tbl,
    function(
      issue_date,
      maturity_date,
      coupon_rate_pct,
      conversion_factor,
      market_yield_pct,
      ...
    ) {
      bond_futures_gross_basis_row_GQL(
        issue_date = issue_date,
        maturity_date = maturity_date,
        coupon_rate_pct = coupon_rate_pct,
        conversion_factor = conversion_factor,
        market_yield_pct = market_yield_pct,
        settlement_date = settlement_date,
        futures_price = futures_price,
        settlement_days = settlement_days,
        calendar = calendar,
        day_counter = day_counter,
        compounding = compounding,
        frequency = frequency
      )
    }
  )

  net_tbl <- purrr::pmap_dfr(
    list(
      bond_obj = gross_tbl$bond_obj,
      conversion_factor = gross_tbl$conversion_factor,
      clean_price = gross_tbl$clean_price,
      dirty_price = gross_tbl$dirty_price,
      gross_basis = gross_tbl$gross_basis
    ),
    function(
      bond_obj,
      conversion_factor,
      clean_price,
      dirty_price,
      gross_basis
    ) {
      bond_futures_net_basis_row_GQL(
        bond_obj = bond_obj,
        conversion_factor = conversion_factor,
        clean_price = clean_price,
        dirty_price = dirty_price,
        gross_basis = gross_basis,
        settlement_date = settlement_date,
        repo_end_date = repo_end_date,
        repo_rate = repo_rate,
        repo_day_counter = repo_day_counter,
        carry_day_counter = carry_day_counter,
        futures_price = futures_price
      )
    }
  )

  duplicate_columns <- intersect(names(gross_tbl), names(net_tbl))

  basket <- dplyr::bind_cols(
    gross_tbl,
    net_tbl |>
      dplyr::select(
        -dplyr::any_of(
          c("bond_obj", duplicate_columns)
        )
      )
  ) |>
    dplyr::mutate(
      ctd = dplyr::row_number() == which.min(.data$net_basis)
    ) |>
    dplyr::arrange(.data$net_basis)

  list(
    basket = basket,
    ctd = basket |>
      dplyr::filter(.data$ctd)
  )
}


#' Build a Black process bundle
#'
#' @param valuation_date QuantLib Date.
#' @param forward Forward or futures price.
#' @param risk_free_rate Risk-free rate.
#' @param volatility Black volatility.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @return List with process and term structures.
#' @export
black_process_bundle_GQL <- function(
    valuation_date,
    forward,
    risk_free_rate,
    volatility,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::NullCalendar()
) {
  discount_curve <- QuantLib::FlatForward(
    valuation_date,
    risk_free_rate,
    day_counter,
    "Continuous"
  )

  volatility_curve <- QuantLib::BlackConstantVol(
    valuation_date,
    calendar,
    volatility,
    day_counter
  )

  discount_handle <- QuantLib::YieldTermStructureHandle(discount_curve)
  volatility_handle <- QuantLib::BlackVolTermStructureHandle(volatility_curve)

  process <- QuantLib::BlackProcess(
    quote_handle_GQL(forward),
    discount_handle,
    volatility_handle
  )

  list(
    process = process,
    discount_curve = discount_curve,
    volatility_curve = volatility_curve,
    discount_handle = discount_handle,
    volatility_handle = volatility_handle
  )
}


#' Build a Black-Scholes-Merton process bundle
#'
#' @param valuation_date QuantLib Date.
#' @param spot Spot price.
#' @param risk_free_rate Risk-free rate.
#' @param dividend_rate Dividend yield.
#' @param volatility Volatility.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @return List with process and term structures.
#' @export
bsm_process_bundle_GQL <- function(
    valuation_date,
    spot,
    risk_free_rate,
    dividend_rate,
    volatility,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET()
) {
  risk_free_curve <- QuantLib::FlatForward(
    valuation_date,
    risk_free_rate,
    day_counter,
    "Continuous"
  )

  dividend_curve <- QuantLib::FlatForward(
    valuation_date,
    dividend_rate,
    day_counter,
    "Continuous"
  )

  volatility_curve <- QuantLib::BlackConstantVol(
    valuation_date,
    calendar,
    volatility,
    day_counter
  )

  process <- QuantLib::BlackScholesMertonProcess(
    quote_handle_GQL(spot),
    QuantLib::YieldTermStructureHandle(dividend_curve),
    QuantLib::YieldTermStructureHandle(risk_free_curve),
    QuantLib::BlackVolTermStructureHandle(volatility_curve)
  )

  list(
    process = process,
    risk_free_curve = risk_free_curve,
    dividend_curve = dividend_curve,
    volatility_curve = volatility_curve
  )
}


#' Build a Black calculator metric table
#'
#' @param payoff QuantLib payoff.
#' @param forward Forward price.
#' @param volatility Volatility.
#' @param maturity Time to maturity.
#' @param discount_factor Discount factor.
#' @return Metric tibble.
#' @export
black_calculator_table_GQL <- function(
    payoff,
    forward,
    volatility,
    maturity,
    discount_factor
) {
  calculator <- QuantLib::BlackCalculator(
    payoff,
    forward,
    volatility * sqrt(maturity),
    discount_factor
  )

  tibble::tribble(
    ~metric, ~value,
    "npv", calculator$value(),
    "delta", calculator$delta(forward),
    "gamma", calculator$gamma(forward),
    "vega", calculator$vega(maturity),
    "theta", calculator$theta(forward, maturity),
    "theta_per_day", calculator$thetaPerDay(forward, maturity)
  )
}


#' Build an option metric table
#'
#' @param option QuantLib option.
#' @param process Optional pricing process.
#' @return Metric tibble.
#' @export
option_metric_table_GQL <- function(option, process = NULL) {
  npv <- safe_num_GQH(option$NPV())

  implied_volatility <- if (is.null(process) || is.na(npv)) {
    NA_real_
  } else {
    safe_num_GQH(
      option$impliedVolatility(npv, process)
    )
  }

  tibble::tribble(
    ~metric, ~value,
    "npv", npv,
    "delta", safe_num_GQH(option$delta()),
    "gamma", safe_num_GQH(option$gamma()),
    "vega", safe_num_GQH(option$vega()),
    "theta", safe_num_GQH(option$theta()),
    "theta_per_day", safe_num_GQH(option$thetaPerDay()),
    "implied_volatility", implied_volatility
  )
}


#' Build a QuantLib path generator
#'
#' @param process QuantLib stochastic process.
#' @param maturity Path maturity.
#' @param n_steps Number of time steps.
#' @param seed Random seed.
#' @param sequence Pseudo-random or Sobol.
#' @return QuantLib path generator.
#' @export
make_path_generator_GQL <- function(
    process,
    maturity,
    n_steps,
    seed = 1L,
    sequence = c("pseudo", "sobol")
) {
  sequence <- match.arg(sequence)

  if (sequence == "pseudo") {
    uniform_rng <- QuantLib::UniformRandomGenerator(as.integer(seed))

    uniform_sequence <- QuantLib::UniformRandomSequenceGenerator(
      as.integer(n_steps),
      uniform_rng
    )

    gaussian_sequence <- QuantLib::GaussianRandomSequenceGenerator(
      uniform_sequence
    )

    return(
      QuantLib::GaussianPathGenerator(
        process,
        maturity,
        as.integer(n_steps),
        gaussian_sequence,
        FALSE
      )
    )
  }

  uniform_sequence <- QuantLib::UniformLowDiscrepancySequenceGenerator(
    as.integer(n_steps),
    as.integer(seed)
  )

  gaussian_sequence <- QuantLib::GaussianLowDiscrepancySequenceGenerator(
    uniform_sequence
  )

  QuantLib::GaussianSobolPathGenerator(
    process,
    maturity,
    as.integer(n_steps),
    gaussian_sequence,
    FALSE
  )
}


#' Generate a path table
#'
#' @param process QuantLib stochastic process.
#' @param maturity Path maturity.
#' @param n_steps Number of steps.
#' @param n_paths Number of paths.
#' @param seed Random seed.
#' @param sequence Pseudo-random or Sobol.
#' @return Long-form path tibble.
#' @export
generate_path_table_GQL <- function(
    process,
    maturity,
    n_steps,
    n_paths,
    seed = 1L,
    sequence = c("pseudo", "sobol")
) {
  sequence <- match.arg(sequence)

  generator <- make_path_generator_GQL(
    process = process,
    maturity = maturity,
    n_steps = n_steps,
    seed = seed,
    sequence = sequence
  )

  purrr::map_dfr(
    seq_len(n_paths),
    function(path_id) {
      one_path <- generator$`next`()$value()
      path_tbl_GQL(one_path, path_id = path_id)
    }
  )
}


#' Price a terminal-payoff option from simulated paths
#'
#' @param path_tbl Long-form path table.
#' @param strike Strike.
#' @param discount_factor Discount factor.
#' @param option_type Call or put.
#' @return List with terminal paths and summary.
#' @export
terminal_option_mc_GQL <- function(
    path_tbl,
    strike,
    discount_factor,
    option_type = c("call", "put")
) {
  option_type <- match.arg(option_type)

  terminal <- path_tbl |>
    dplyr::group_by(.data$path_id) |>
    dplyr::slice_max(
      order_by = .data$step,
      n = 1L,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      payoff = if (option_type == "call") {
        pmax(.data$price - strike, 0)
      } else {
        pmax(strike - .data$price, 0)
      },
      discounted_payoff = discount_factor * .data$payoff
    )

  summary <- terminal |>
    dplyr::summarise(
      paths = dplyr::n(),
      npv = mean(.data$discounted_payoff),
      standard_error = stats::sd(.data$discounted_payoff) /
        sqrt(dplyr::n())
    )

  list(
    terminal = terminal,
    summary = summary
  )
}


#' Approximate an American put with Longstaff-Schwartz
#'
#' @param path_tbl Long-form path table.
#' @param strike Strike.
#' @param risk_free_rate Continuously compounded discount rate.
#' @return List with summary, exercise table, and diagnostics.
#' @export
lsm_american_put_GQL <- function(
    path_tbl,
    strike,
    risk_free_rate
) {
  ordered <- path_tbl |>
    dplyr::arrange(.data$path_id, .data$step)

  path_ids <- unique(ordered$path_id)

  time_grid <- ordered |>
    dplyr::filter(.data$path_id == path_ids[[1L]]) |>
    dplyr::pull(.data$time)

  n_paths <- length(path_ids)
  n_points <- length(time_grid)

  if (n_points < 2L) {
    stop("Each path must contain at least two points.", call. = FALSE)
  }

  if (nrow(ordered) != n_paths * n_points) {
    stop("All paths must have the same number of points.", call. = FALSE)
  }

  price_matrix <- matrix(
    ordered$price,
    nrow = n_paths,
    ncol = n_points,
    byrow = TRUE
  )

  n_steps <- n_points - 1L

  initial_state <- list(
    cashflow = pmax(strike - price_matrix[, n_steps + 1L], 0),
    exercise_step = rep(n_steps, n_paths),
    diagnostics = list()
  )

  backward_steps <- if (n_steps <= 1L) {
    integer()
  } else {
    rev(seq_len(n_steps - 1L))
  }

  final_state <- purrr::reduce(
    backward_steps,
    function(state, step_index) {
      spot_now <- price_matrix[, step_index + 1L]
      immediate_value <- pmax(strike - spot_now, 0)

      exercise_time <- time_grid[
        state$exercise_step + 1L
      ]

      continuation_realized <- state$cashflow *
        exp(
          -risk_free_rate *
            (
              exercise_time -
                time_grid[[step_index + 1L]]
            )
        )

      itm_index <- which(immediate_value > 0)
      continuation_estimate <- rep(Inf, n_paths)

      if (length(itm_index) >= 3L) {
        regression_tbl <- tibble::tibble(
          spot = spot_now[itm_index],
          continuation = continuation_realized[itm_index]
        )

        fit <- stats::lm(
          continuation ~ spot + I(spot^2),
          data = regression_tbl
        )

        fitted <- stats::predict(
          fit,
          newdata = tibble::tibble(
            spot = spot_now[itm_index]
          )
        )

        continuation_estimate[itm_index] <- pmax(fitted, 0)
      }

      exercise_now <- immediate_value > 0 &
        immediate_value > continuation_estimate

      diagnostic <- tibble::tibble(
        step = step_index,
        time = time_grid[[step_index + 1L]],
        in_the_money_paths = length(itm_index),
        exercised_paths = sum(exercise_now),
        mean_immediate_value = mean(immediate_value),
        mean_realized_continuation = mean(continuation_realized)
      )

      list(
        cashflow = dplyr::if_else(
          exercise_now,
          immediate_value,
          state$cashflow
        ),
        exercise_step = dplyr::if_else(
          exercise_now,
          step_index,
          state$exercise_step
        ),
        diagnostics = append(
          state$diagnostics,
          list(diagnostic)
        )
      )
    },
    .init = initial_state
  )

  present_value <- final_state$cashflow *
    exp(
      -risk_free_rate *
        time_grid[
          final_state$exercise_step + 1L
        ]
    )

  exercise <- tibble::tibble(
    path_id = path_ids,
    exercise_step = final_state$exercise_step,
    exercise_time = time_grid[
      final_state$exercise_step + 1L
    ],
    exercise_price = price_matrix[
      cbind(
        seq_len(n_paths),
        final_state$exercise_step + 1L
      )
    ],
    cashflow = final_state$cashflow,
    present_value = present_value
  )

  list(
    summary = tibble::tribble(
      ~metric, ~value,
      "lsm_npv", mean(present_value),
      "standard_error", stats::sd(present_value) / sqrt(n_paths),
      "paths", n_paths,
      "steps", n_steps
    ),
    exercise = exercise,
    diagnostics = dplyr::bind_rows(final_state$diagnostics)
  )
}


#' Price an option under the normal model
#'
#' @param option_sign 1 for call, -1 for put.
#' @param forward Forward.
#' @param strike Strike.
#' @param vol Normal volatility.
#' @param maturity Maturity.
#' @param discount_factor Discount factor.
#' @return Numeric option value.
#' @export
normal_option_price_GQL <- function(
    option_sign,
    forward,
    strike,
    vol,
    maturity,
    discount_factor
) {
  intrinsic_value <- max(
    option_sign * (forward - strike),
    0
  )

  if (vol <= 0 || maturity <= 0) {
    return(discount_factor * intrinsic_value)
  }

  standard_deviation <- vol * sqrt(maturity)

  if (abs(standard_deviation) < 1e-15) {
    return(discount_factor * intrinsic_value)
  }

  d_value <- option_sign *
    (forward - strike) /
    standard_deviation

  discount_factor *
    standard_deviation *
    (
      d_value * stats::pnorm(d_value) +
        stats::dnorm(d_value)
    )
}


#' Calculate normal-model option Greeks
#'
#' @param option_sign 1 for call, -1 for put.
#' @param forward Forward.
#' @param strike Strike.
#' @param vol Normal volatility.
#' @param maturity Maturity.
#' @param discount_factor Discount factor.
#' @param risk_free_rate Risk-free rate.
#' @return Metric tibble.
#' @export
normal_option_greeks_GQL <- function(
    option_sign,
    forward,
    strike,
    vol,
    maturity,
    discount_factor,
    risk_free_rate
) {
  npv <- normal_option_price_GQL(
    option_sign,
    forward,
    strike,
    vol,
    maturity,
    discount_factor
  )

  if (vol <= 0 || maturity <= 0) {
    return(
      tibble::tribble(
        ~metric, ~value,
        "npv", npv,
        "delta", NA_real_,
        "gamma", NA_real_,
        "vega", NA_real_,
        "theta", NA_real_
      )
    )
  }

  standard_deviation <- vol * sqrt(maturity)
  d_value <- option_sign *
    (forward - strike) /
    standard_deviation

  tibble::tribble(
    ~metric, ~value,
    "npv", npv,
    "delta", option_sign * discount_factor * stats::pnorm(d_value),
    "gamma", discount_factor * stats::dnorm(d_value) / standard_deviation,
    "vega", discount_factor * sqrt(maturity) * stats::dnorm(d_value),
    "theta",
    risk_free_rate * npv -
      0.5 *
      discount_factor *
      stats::dnorm(d_value) *
      vol /
      sqrt(maturity)
  )
}


#' Build a normal-model pricing closure
#'
#' @param option_sign 1 for call, -1 for put.
#' @param strike Strike.
#' @param maturity Maturity.
#' @param discount_factor Discount factor.
#' @param forward Forward.
#' @param vol Normal volatility.
#' @return Pricing function.
#' @export
make_normal_calculator_GQL <- function(
    option_sign,
    strike,
    maturity,
    discount_factor,
    forward,
    vol
) {
  function(
      forward_new = forward,
      vol_new = vol,
      maturity_new = maturity,
      discount_factor_new = discount_factor
  ) {
    normal_option_price_GQL(
      option_sign = option_sign,
      forward = forward_new,
      strike = strike,
      vol = vol_new,
      maturity = maturity_new,
      discount_factor = discount_factor_new
    )
  }
}


#' Build a class-like normal calculator
#'
#' @inheritParams make_normal_calculator_GQL
#' @return Object with an npv method.
#' @export
normal_calculator_GQL <- function(
    option_sign,
    strike,
    maturity,
    discount_factor,
    forward,
    vol
) {
  structure(
    list(
      npv = make_normal_calculator_GQL(
        option_sign,
        strike,
        maturity,
        discount_factor,
        forward,
        vol
      )
    ),
    class = "normal_calculator_GQL"
  )
}


.phi_tilde_GQL <- function(x) {
  stats::pnorm(x) +
    stats::dnorm(x) / x
}


.inverse_phi_tilde_GQL <- function(phi_tilde_star) {
  if (phi_tilde_star < -0.001882039271) {
    g_value <- 1 / (phi_tilde_star - 0.5)

    xi_bar <- (
      0.032114372355 -
        g_value^2 *
        (
          0.016969777977 -
            g_value^2 *
            (
              2.6207332461e-3 -
                9.6066952861e-5 * g_value^2
            )
        )
    ) /
      (
        1 -
          g_value^2 *
          (
            0.6635646938 -
              g_value^2 *
              (
                0.14528712196 -
                  0.010472855461 * g_value^2
              )
          )
      )

    x_bar <- g_value *
      (
        0.3989422804014326 +
          xi_bar * g_value^2
      )
  } else {
    h_value <- sqrt(-log(-phi_tilde_star))

    x_bar <- (
      9.4883409779 -
        h_value *
        (
          9.6320903635 -
            h_value *
            (
              0.58556997323 +
                2.1464093351 * h_value
            )
        )
    ) /
      (
        1 -
          h_value *
          (
            0.65174820867 +
              h_value *
              (
                1.5120247828 +
                  6.6437847132e-5 * h_value
              )
          )
      )
  }

  q_value <- (
    .phi_tilde_GQL(x_bar) -
      phi_tilde_star
  ) /
    stats::dnorm(x_bar)

  x_bar +
    3 *
    q_value *
    x_bar^2 *
    (
      2 -
        q_value *
        x_bar *
        (
          2 +
            x_bar^2
        )
    ) /
    (
      6 +
        q_value *
        x_bar *
        (
          -12 +
            x_bar *
            (
              6 * q_value +
                x_bar *
                (
                  -6 +
                    q_value *
                    x_bar *
                    (
                      3 +
                        x_bar^2
                    )
                )
            )
        )
    )
}


#' Infer normal volatility from an option price
#'
#' @param option_sign 1 for call, -1 for put.
#' @param strike Strike.
#' @param forward Forward.
#' @param maturity Maturity.
#' @param option_npv Option NPV.
#' @param discount_factor Discount factor.
#' @return Normal volatility.
#' @export
normal_vol_from_price_GQL <- function(
    option_sign,
    strike,
    forward,
    maturity,
    option_npv,
    discount_factor
) {
  undiscounted_npv <- option_npv / discount_factor

  if (abs(strike - forward) < 1e-15) {
    return(
      undiscounted_npv /
        (
          sqrt(maturity) *
            stats::dnorm(0)
        )
    )
  }

  intrinsic_value <- max(
    option_sign * (forward - strike),
    0
  )

  time_value <- undiscounted_npv - intrinsic_value

  if (abs(time_value) < 1e-15) {
    return(0)
  }

  phi_tilde_target <- -abs(
    time_value / (strike - forward)
  )

  x_star <- .inverse_phi_tilde_GQL(
    phi_tilde_target
  )

  abs(
    (strike - forward) /
      (
        x_star *
          sqrt(maturity)
      )
  )
}


#' Calculate swap annuity from a schedule
#'
#' @param schedule QuantLib Schedule.
#' @param curve QuantLib yield curve.
#' @param day_counter QuantLib day counter.
#' @return Numeric annuity.
#' @export
swap_annuity_from_schedule_GQL <- function(
    schedule,
    curve,
    day_counter
) {
  dates <- .schedule_date_vector_ql_GQL(schedule)

  if (length(dates) < 2L) {
    return(NA_real_)
  }

  components <- purrr::map_dbl(
    seq_len(length(dates) - 1L),
    function(i) {
      accrual <- day_counter$yearFraction(
        dates[[i]],
        dates[[i + 1L]]
      )

      discount <- curve_discount_safe_GQL(
        curve,
        dates[[i + 1L]]
      )

      accrual * discount
    }
  )

  sum(components, na.rm = TRUE)
}


#' Calculate a forward swap rate from a schedule
#'
#' @param schedule QuantLib Schedule.
#' @param curve QuantLib yield curve.
#' @param day_counter QuantLib day counter.
#' @return Numeric forward swap rate.
#' @export
forward_swap_rate_from_schedule_GQL <- function(
    schedule,
    curve,
    day_counter
) {
  dates <- .schedule_date_vector_ql_GQL(schedule)

  annuity <- swap_annuity_from_schedule_GQL(
    schedule,
    curve,
    day_counter
  )

  (
    curve_discount_safe_GQL(curve, dates[[1L]]) -
      curve_discount_safe_GQL(curve, dates[[length(dates)]])
  ) /
    annuity
}


#' Calculate root mean squared error
#'
#' @param actual Actual values.
#' @param fitted Fitted values.
#' @return Numeric RMSE.
#' @export
rmse_GQH <- function(actual, fitted) {
  sqrt(mean((actual - fitted)^2))
}


#' Safely calculate Black SABR volatility
#'
#' @param strike Strike.
#' @param forward Forward.
#' @param maturity Maturity.
#' @param alpha SABR alpha.
#' @param beta SABR beta.
#' @param volvol SABR volatility of volatility.
#' @param rho SABR correlation.
#' @return Numeric volatility or NA.
#' @export
safe_sabr_vol_GQL <- function(
    strike,
    forward,
    maturity,
    alpha,
    beta,
    volvol,
    rho
) {
  tryCatch(
    QuantLib::sabrVolatility(
      strike,
      forward,
      maturity,
      alpha,
      beta,
      volvol,
      rho
    ),
    error = function(e) NA_real_
  )
}


#' Approximate an objective gradient
#'
#' @param parameters Parameter vector.
#' @param objective_function Objective function.
#' @param epsilon Finite-difference step.
#' @return Numeric gradient.
#' @export
approx_gradient_GQH <- function(
    parameters,
    objective_function,
    epsilon = 1e-8
) {
  base_value <- objective_function(parameters)

  purrr::map_dbl(
    seq_along(parameters),
    function(i) {
      parameters_up <- parameters
      parameters_up[[i]] <- parameters_up[[i]] + epsilon

      (
        objective_function(parameters_up) -
          base_value
      ) /
        epsilon
    }
  )
}


#' Calculate Hagan normal SABR volatility
#'
#' @param strike Strike.
#' @param forward Forward.
#' @param maturity Maturity.
#' @param beta SABR beta.
#' @param alpha SABR alpha.
#' @param volvol SABR volatility of volatility.
#' @param rho SABR correlation.
#' @return Numeric normal volatility.
#' @export
normal_vol_hagan_GQL <- function(
    strike,
    forward,
    maturity,
    beta,
    alpha,
    volvol,
    rho
) {
  log_fk <- log(forward / strike)

  first_adjustment <- 1 +
    log_fk^2 / 24 +
    log_fk^4 / 1920

  second_adjustment <- 1 +
    (1 - beta)^2 * log_fk^2 / 24 +
    (1 - beta)^4 * log_fk^4 / 1920

  leading_term <- alpha *
    (forward * strike)^(beta / 2) *
    first_adjustment /
    second_adjustment

  z_value <- (volvol / alpha) *
    (forward * strike)^((1 - beta) / 2) *
    log_fk

  x_value <- log(
    (
      sqrt(
        1 -
          2 * rho * z_value +
          z_value^2
      ) -
        rho +
        z_value
    ) /
      (1 - rho)
  )

  z_over_x <- if (abs(z_value) > 1e-7) {
    z_value / x_value
  } else {
    1
  }

  correction_1 <- -beta *
    (2 - beta) *
    alpha^2 /
    (
      24 *
        (forward * strike)^(1 - beta)
    )

  correction_2 <- rho *
    alpha *
    volvol *
    beta /
    (
      4 *
        (forward * strike)^((1 - beta) / 2)
    )

  correction_3 <- (
    2 -
      3 * rho^2
  ) *
    volvol^2 /
    24

  leading_term *
    z_over_x *
    (
      1 +
        (
          correction_1 +
            correction_2 +
            correction_3
        ) *
        maturity
    )
}


#' Calculate shifted Hagan normal SABR volatility
#'
#' @inheritParams normal_vol_hagan_GQL
#' @param shift Shift applied to strike and forward.
#' @return Numeric normal volatility or NA.
#' @export
shifted_normal_vol_hagan_GQL <- function(
    strike,
    forward,
    maturity,
    beta,
    alpha,
    volvol,
    rho,
    shift = 0.025
) {
  shifted_strike <- strike + shift
  shifted_forward <- forward + shift

  if (shifted_strike <= 0 || shifted_forward <= 0) {
    return(NA_real_)
  }

  normal_vol_hagan_GQL(
    strike = shifted_strike,
    forward = shifted_forward,
    maturity = maturity,
    beta = beta,
    alpha = alpha,
    volvol = volvol,
    rho = rho
  )
}


#' Safely obtain a callable bond clean price
#'
#' @param bond QuantLib callable bond.
#' @return Numeric price or NA.
#' @export
callable_bond_clean_price_safe_GQL <- function(bond) {
  attempts <- list(
    function() bond$cleanPrice(),
    function() bond$settlementValue(),
    function() bond$NPV()
  )

  result <- purrr::detect(
    purrr::map(
      attempts,
      ~ tryCatch(.x(), error = function(e) NULL)
    ),
    ~ !is.null(.x)
  )

  if (is.null(result)) {
    return(NA_real_)
  }

  as.numeric(result)
}


#' Safely obtain a callable bond dirty price
#'
#' @param bond QuantLib callable bond.
#' @return Numeric price or NA.
#' @export
callable_bond_dirty_price_safe_GQL <- function(bond) {
  tryCatch(
    as.numeric(bond$dirtyPrice()),
    error = function(e) NA_real_
  )
}


#' Safely obtain a callable bond settlement value
#'
#' @param bond QuantLib callable bond.
#' @return Numeric value or NA.
#' @export
callable_bond_settlement_value_safe_GQL <- function(bond) {
  tryCatch(
    as.numeric(bond$settlementValue()),
    error = function(e) NA_real_
  )
}


#' Build a QuantLib callability schedule
#'
#' @param call_dates Character, Date, or QuantLib dates.
#' @param call_price_clean Clean call price.
#' @return QuantLib CallabilitySchedule.
#' @export
make_callability_schedule_GQL <- function(
    call_dates,
    call_price_clean = 100
) {
  schedule <- QuantLib::CallabilitySchedule()

  call_price <- QuantLib::BondPrice(
    call_price_clean,
    QuantLib::BondPrice_Clean_get()
  )

  purrr::walk(
    call_dates,
    function(call_date) {
      call_date_ql <- if (
        is.character(call_date) ||
          inherits(call_date, "Date")
      ) {
        date_GQL(call_date)
      } else {
        call_date
      }

      callability <- QuantLib::Callability(
        call_price,
        QuantLib::Callability_Call_get(),
        call_date_ql
      )

      QuantLib::CallabilitySchedule_append(
        schedule,
        callability
      )
    }
  )

  schedule
}


#' Calculate Vasicek B
#'
#' @param time Start time.
#' @param maturity Maturity.
#' @param mean_reversion Mean reversion.
#' @return Numeric B.
#' @export
vasicek_b_GQL <- function(
    time,
    maturity,
    mean_reversion
) {
  time_to_maturity <- maturity - time

  if (abs(mean_reversion) < 1e-12) {
    return(time_to_maturity)
  }

  (
    1 -
      exp(
        -mean_reversion *
          time_to_maturity
      )
  ) /
    mean_reversion
}


#' Calculate Vasicek A
#'
#' @inheritParams vasicek_b_GQL
#' @param sigma Short-rate volatility.
#' @param long_run_rate Long-run rate.
#' @return Numeric A.
#' @export
vasicek_a_GQL <- function(
    time,
    maturity,
    mean_reversion,
    sigma,
    long_run_rate
) {
  time_to_maturity <- maturity - time

  b_value <- vasicek_b_GQL(
    time,
    maturity,
    mean_reversion
  )

  (
    sigma^2 /
      (2 * mean_reversion^2)
  ) *
    (
      time_to_maturity -
        b_value
    ) -
    (
      sigma^2 *
        b_value^2
    ) /
    (
      4 *
        mean_reversion
    ) -
    long_run_rate *
    (
      time_to_maturity -
        b_value
    )
}


#' Calculate a Vasicek zero rate
#'
#' @inheritParams vasicek_a_GQL
#' @param short_rate Current short rate.
#' @return Numeric zero rate.
#' @export
vasicek_zero_rate_GQL <- function(
    time,
    maturity,
    mean_reversion,
    sigma,
    long_run_rate,
    short_rate
) {
  if (abs(maturity - time) < 1e-12) {
    return(short_rate)
  }

  a_value <- vasicek_a_GQL(
    time,
    maturity,
    mean_reversion,
    sigma,
    long_run_rate
  )

  b_value <- vasicek_b_GQL(
    time,
    maturity,
    mean_reversion
  )

  -(
    a_value -
      short_rate *
      b_value
  ) /
    (
      maturity -
        time
    )
}


#' Calculate Hull-White B
#'
#' @param time Start time.
#' @param maturity Maturity.
#' @param mean_reversion Mean reversion.
#' @return Numeric B.
#' @export
hull_white_b_GQL <- function(
    time,
    maturity,
    mean_reversion
) {
  time_to_maturity <- maturity - time

  if (abs(mean_reversion) < 1e-12) {
    return(time_to_maturity)
  }

  (
    1 -
      exp(
        -mean_reversion *
          time_to_maturity
      )
  ) /
    mean_reversion
}


#' Calculate Hull-White variance term
#'
#' @inheritParams hull_white_b_GQL
#' @param sigma Short-rate volatility.
#' @return Numeric variance term.
#' @export
hull_white_variance_GQL <- function(
    time,
    maturity,
    mean_reversion,
    sigma
) {
  time_to_maturity <- maturity - time

  (
    sigma^2 /
      mean_reversion^2
  ) *
    (
      time_to_maturity +
        (2 / mean_reversion) *
        exp(
          -mean_reversion *
            time_to_maturity
        ) -
        (
          1 /
            (2 * mean_reversion)
        ) *
        exp(
          -2 *
            mean_reversion *
            time_to_maturity
        ) -
        3 /
        (
          2 *
            mean_reversion
        )
    )
}


#' Calculate Hull-White A
#'
#' @inheritParams hull_white_variance_GQL
#' @return Numeric A.
#' @export
hull_white_a_GQL <- function(
    time,
    maturity,
    mean_reversion,
    sigma
) {
  0.5 *
    (
      hull_white_variance_GQL(
        time,
        maturity,
        mean_reversion,
        sigma
      ) -
        hull_white_variance_GQL(
          0,
          maturity,
          mean_reversion,
          sigma
        ) +
        hull_white_variance_GQL(
          0,
          time,
          mean_reversion,
          sigma
        )
    )
}


#' Calculate a Hull-White zero rate
#'
#' @param time Start time.
#' @param maturity Maturity.
#' @param curve Initial yield curve.
#' @param mean_reversion Mean reversion.
#' @param sigma Short-rate volatility.
#' @param state_variable Hull-White state variable.
#' @return Numeric zero rate.
#' @export
hull_white_zero_rate_GQL <- function(
    time,
    maturity,
    curve,
    mean_reversion,
    sigma,
    state_variable = 0
) {
  if (abs(maturity - time) < 1e-12) {
    small_time <- 0.0001

    initial_curve_rate <- tryCatch(
      -log(curve$discount(small_time)) /
        small_time,
      error = function(e) 0
    )

    return(
      initial_curve_rate +
        state_variable
    )
  }

  maturity_discount_factor <- curve_discount_safe_GQL(
    curve,
    maturity
  )

  time_discount_factor <- curve_discount_safe_GQL(
    curve,
    time
  )

  if (
    !is.finite(maturity_discount_factor) ||
      !is.finite(time_discount_factor) ||
      maturity_discount_factor <= 0 ||
      time_discount_factor <= 0
  ) {
    return(NA_real_)
  }

  a_value <- hull_white_a_GQL(
    time,
    maturity,
    mean_reversion,
    sigma
  )

  b_value <- hull_white_b_GQL(
    time,
    maturity,
    mean_reversion
  )

  -(
    log(
      maturity_discount_factor /
        time_discount_factor
    ) +
      a_value -
      state_variable *
      b_value
  ) /
    (
      maturity -
        time
    )
}


#' Calculate an approximate Hull-White forward-measure drift
#'
#' @param start_time Start time.
#' @param exercise_time Exercise time.
#' @param maturity Maturity.
#' @param mean_reversion Mean reversion.
#' @param sigma Short-rate volatility.
#' @return Numeric drift adjustment.
#' @export
hull_white_forward_drift_GQL <- function(
    start_time,
    exercise_time,
    maturity,
    mean_reversion,
    sigma
) {
  if (abs(mean_reversion) < 1e-12) {
    return(
      sigma^2 / 2 *
        (exercise_time - start_time) *
        (2 * maturity - exercise_time - start_time)
    )
  }

  coefficient <- sigma^2 / mean_reversion^2

  exp_1 <- exp(
    -mean_reversion *
      (exercise_time - start_time)
  )

  exp_2 <- exp(
    -mean_reversion *
      (maturity - exercise_time)
  )

  exp_3 <- exp(
    -mean_reversion *
      (maturity + exercise_time - 2 * start_time)
  )

  coefficient * (1 - exp_1) -
    0.5 * coefficient * (exp_2 - exp_3)
}


.mid_date_ql_GQH <- function(first_date, second_date) {
  first_r <- as_r_date_GQH(first_date)
  second_r <- as_r_date_GQH(second_date)

  midpoint <- first_r +
    floor(
      as.numeric(
        second_r -
          first_r
      ) /
        2
    )

  date_GQL(
    format(
      midpoint,
      "%Y-%m-%d"
    )
  )
}

#' Calculate a midpoint date
#'
#' @param first_date First QuantLib date, R Date, or ISO date string.
#' @param second_date Second QuantLib date, R Date, or ISO date string.
#' @return An R Date.
#' @export
mid_date_GQH <- function(first_date, second_date) {
  as_r_date_GQH(
    .mid_date_ql_GQH(
      first_date = first_date,
      second_date = second_date
    )
  )
}

#' Obtain the QuantLib CDS buyer-side enum
#'
#' @return QuantLib protection buyer enum.
#' @export
cds_buyer_side_GQL <- function() {
  side <- tryCatch(
    QuantLib::Side_Buyer_get(),
    error = function(e) NULL
  )

  if (!is.null(side)) {
    return(side)
  }

  side <- tryCatch(
    QuantLib::Protection_Buyer_get(),
    error = function(e) NULL
  )

  if (!is.null(side)) {
    return(side)
  }

  stop(
    "Could not find a CDS buyer-side enum in this QuantLib build.",
    call. = FALSE
  )
}


#' Build a CDS cashflow and probability table
#'
#' @param cds_schedule QuantLib CDS schedule.
#' @param protection_start_date Protection start date.
#' @param coupon_rate Running coupon rate.
#' @param hazard_curve Hazard curve.
#' @param discount_curve Discount curve.
#' @param trade_date Trade date.
#' @param notional Notional.
#' @return CDS cashflow table.
#' @export
cds_cashflow_table_GQL <- function(
    cds_schedule,
    protection_start_date,
    coupon_rate,
    hazard_curve,
    discount_curve,
    trade_date,
    notional
) {
  schedule_dates <- .schedule_date_vector_ql_GQL(
    cds_schedule
  )

  number_of_dates <- length(schedule_dates)

  if (number_of_dates < 2L) {
    stop(
      "cds_schedule must contain at least two dates.",
      call. = FALSE
    )
  }

  payment_dates <- c(
    list(protection_start_date),
    schedule_dates[-1L]
  )

  accrual_start_dates <- c(
    list(NULL),
    schedule_dates[-number_of_dates]
  )

  accrual_end_dates <- payment_dates

  accrual_rows <- purrr::map2(
    accrual_start_dates,
    accrual_end_dates,
    function(start_date, end_date) {
      if (is.null(start_date) || is.null(end_date)) {
        return(
          list(
            accrual_days = NA_real_,
            accrual_year_fraction_365 = NA_real_,
            coupon_amount = NA_real_
          )
        )
      }

      start_r <- as.Date(iso_GQL(start_date))
      end_r <- as.Date(iso_GQL(end_date))
      days <- as.numeric(end_r - start_r)

      list(
        accrual_days = days,
        accrual_year_fraction_365 = days / 365,
        coupon_amount = notional *
          coupon_rate *
          days /
          360
      )
    }
  )

  trade_date_r <- as.Date(iso_GQL(trade_date))

  curve_rows <- purrr::map(
    accrual_end_dates,
    function(end_date) {
      if (is.null(end_date)) {
        return(
          list(
            discount_factor = 1,
            survival_probability = 1
          )
        )
      }

      end_date_r <- as.Date(iso_GQL(end_date))

      if (end_date_r <= trade_date_r) {
        return(
          list(
            discount_factor = 1,
            survival_probability = 1
          )
        )
      }

      list(
        discount_factor = discount_curve$discount(end_date),
        survival_probability = hazard_curve$survivalProbability(end_date)
      )
    }
  )

  survival_probabilities <- purrr::map_dbl(
    curve_rows,
    "survival_probability"
  )

  default_probabilities <- c(
    0,
    -diff(survival_probabilities)
  )

  midpoint_dates <- c(
    list(NULL),
    purrr::map2(
      payment_dates[-number_of_dates],
      accrual_end_dates[-1L],
      function(first_date, second_date) {
        if (is.null(first_date) || is.null(second_date)) {
          return(NULL)
        }

        .mid_date_ql_GQH(
          first_date,
          second_date
        )
      }
    )
  )

  midpoint_rows <- purrr::map(
    midpoint_dates,
    function(midpoint_date) {
      if (is.null(midpoint_date)) {
        return(
          list(
            discount_factor = NA_real_,
            survival_probability = NA_real_
          )
        )
      }

      list(
        discount_factor = discount_curve$discount(midpoint_date),
        survival_probability = hazard_curve$survivalProbability(midpoint_date)
      )
    }
  )

  tibble::tibble(
    payment_date = purrr::map_chr(
      payment_dates,
      safe_iso_GQH
    ),
    coupon_rate = c(
      NA_real_,
      rep(coupon_rate, number_of_dates - 1L)
    ),
    accrual_start = purrr::map_chr(
      accrual_start_dates,
      safe_iso_GQH
    ),
    accrual_end = purrr::map_chr(
      accrual_end_dates,
      safe_iso_GQH
    ),
    accrual_days = purrr::map_dbl(
      accrual_rows,
      "accrual_days"
    ),
    accrual_year_fraction_365 = purrr::map_dbl(
      accrual_rows,
      "accrual_year_fraction_365"
    ),
    coupon_amount = purrr::map_dbl(
      accrual_rows,
      "coupon_amount"
    ),
    discount_factor = purrr::map_dbl(
      curve_rows,
      "discount_factor"
    ),
    survival_probability = survival_probabilities,
    default_probability = default_probabilities,
    midpoint_date = purrr::map_chr(
      midpoint_dates,
      safe_iso_GQH
    ),
    midpoint_discount_factor = purrr::map_dbl(
      midpoint_rows,
      "discount_factor"
    ),
    midpoint_survival_probability = purrr::map_dbl(
      midpoint_rows,
      "survival_probability"
    )
  )
}


#' Build an ISDA CDS pricing engine
#'
#' @param hazard_curve_handle Default probability curve handle.
#' @param recovery_rate Recovery rate.
#' @param discount_curve_handle Discount curve handle.
#' @return QuantLib IsdaCdsEngine.
#' @export
cds_isda_engine_GQL <- function(
    hazard_curve_handle,
    recovery_rate,
    discount_curve_handle
) {
  QuantLib::IsdaCdsEngine(
    hazard_curve_handle,
    recovery_rate,
    discount_curve_handle
  )
}

#' Add a local QuantLib library path
#'
#' @param path Local library path containing the SWIG QuantLib package.
#'
#' @return The updated library search paths, invisibly.
#' @export
use_quantlib_GQH <- function(
  path = "C:/AnalyticFin/Projects/QuantLib_tidy/library/windows"
) {
  .libPaths(c(path, .libPaths()))
  invisible(.libPaths())
}

#' Test GaussQuant
#'
#' @export
hello_GQH <- function() {
  "GaussQuant is ready."
}
