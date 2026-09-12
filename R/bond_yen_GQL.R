#' Year fraction for yen-denominated bond calculations
#'
#' Computes the year fraction used by the simplified yen-bond pricing
#' functions in this file. Complete anniversary years are counted as whole
#' years; the residual period is Actual/365. A February 29 anniversary is
#' treated as February 28.
#'
#' @param start_date Settlement or calculation date.
#' @param end_date Maturity or end date.
#' @return A numeric year fraction.
#' @export
bond_year_fraction_YEN_GQL <- function(start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (length(start_date) != 1L || length(end_date) != 1L ||
      is.na(start_date) || is.na(end_date) || end_date < start_date) {
    stop("`start_date` and `end_date` must be scalar dates with start_date <= end_date.",
         call. = FALSE)
  }

  year_difference <- lubridate::year(end_date) - lubridate::year(start_date)
  result <- if (year_difference > 1L) {
    start_day <- dplyr::if_else(
      lubridate::month(start_date) == 2L && lubridate::day(start_date) == 29L,
      28L, lubridate::day(start_date)
    )
    end_day <- dplyr::if_else(
      lubridate::month(end_date) == 2L && lubridate::day(end_date) == 29L,
      28L, lubridate::day(end_date)
    )
    reference_start <- lubridate::make_date(
      1990L, lubridate::month(start_date), start_day
    )
    reference_end <- lubridate::make_date(
      1991L, lubridate::month(end_date), end_day
    )
    as.numeric(reference_end - reference_start) / 365 +
      year_difference - 1
  } else {
    as.numeric(end_date - start_date) / 365
  }
  truncate_decimal_YEN_GQH(result, 7L)
}

#' Price a yen-denominated bond from a simple rate
#'
#' @param start_date Settlement date.
#' @param end_date Maturity date.
#' @param coupon_rate Annual coupon amount per 100 face value, in percent.
#' @param simple_rate Annual simple rate, in percent.
#' @param redemption Redemption amount per 100 face value.
#' @param digits Optional number of decimal places to truncate to. Use `NULL`
#'   for the untruncated result.
#' @return Bond price per 100 face value.
#' @export
bond_price_from_simple_rate_YEN_GQL <- function(
    start_date, end_date, coupon_rate, simple_rate,
    redemption = 100, digits = 3L) {
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  denominator <- 1 + simple_rate * time / 100
  if (!is.finite(denominator) || denominator <= 0) {
    stop("The simple rate produces a non-positive discount denominator.", call. = FALSE)
  }
  price <- (redemption + coupon_rate * time) / denominator
  truncate_decimal_YEN_GQH(price, digits)
}

#' Solve a yen-denominated bond simple rate from price
#'
#' @inheritParams bond_price_from_simple_rate_YEN_GQL
#' @param price Bond price per 100 face value.
#' @param max_iterations Maximum number of Newton iterations.
#' @return Annual simple rate in percent.
#' @export
bond_simple_rate_from_price_YEN_GQL <- function(
    start_date, end_date, coupon_rate, price,
    redemption = 100, digits = 7L, max_iterations = 100L) {
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  if (!is.finite(price) || price <= 0 || time <= 0) {
    stop("`price` and the time to maturity must be positive.", call. = FALSE)
  }
  numerator <- redemption + coupon_rate * time
  initial_rate <- 100 / time * (numerator / price - 1)
  price_function <- function(rate) numerator / (1 + rate * time / 100)
  derivative_function <- function(rate) {
    -numerator * (time / 100) / (1 + rate * time / 100)^2
  }
  rate <- newton_rate_YEN_GQH(
    price_function, derivative_function, price, initial_rate,
    digits, max_iterations
  )
  truncate_decimal_YEN_GQH(rate, digits)
}

#' Price a yen-denominated bond from a compound yield
#'
#' This implements the geometric-series convention in which coupon payments
#' occur `coupon_frequency` times per year and the quoted yield compounds
#' `compound_frequency` times per year.
#'
#' @inheritParams bond_price_from_simple_rate_YEN_GQL
#' @param compound_yield Annual compound yield, in percent.
#' @param coupon_frequency Number of coupon payments per year.
#' @param compound_frequency Number of yield-compounding periods per year.
#' @return Bond price per 100 face value.
#' @export
bond_price_from_compound_yield_YEN_GQL <- function(
    start_date, end_date, coupon_rate, compound_yield,
    redemption = 100, coupon_frequency = 2,
    compound_frequency = 2, digits = 3L) {
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  validate_frequencies_YEN_GQH(coupon_frequency, compound_frequency)
  base <- 1 + compound_yield / (100 * compound_frequency)
  if (!is.finite(base) || base <= 0) {
    stop("The compound yield must be greater than -100 * compound_frequency.",
         call. = FALSE)
  }

  values <- compound_values_YEN_GQH(
    time, coupon_rate, compound_yield, redemption,
    coupon_frequency, compound_frequency
  )
  truncate_decimal_YEN_GQH(values$price, digits)
}

#' Solve a yen-denominated bond compound yield from price
#'
#' @inheritParams bond_price_from_compound_yield_YEN_GQL
#' @param price Bond price per 100 face value.
#' @param max_iterations Maximum number of Newton iterations.
#' @return Annual compound yield in percent.
#' @export
bond_compound_yield_from_price_YEN_GQL <- function(
    start_date, end_date, coupon_rate, price,
    redemption = 100, coupon_frequency = 2,
    compound_frequency = 2, digits = 7L, max_iterations = 100L) {
  if (!is.finite(price) || price <= 0) {
    stop("`price` must be positive.", call. = FALSE)
  }
  validate_frequencies_YEN_GQH(coupon_frequency, compound_frequency)
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  initial_rate <- 100 / time * ((redemption + coupon_rate * time) / price - 1)
  values <- function(rate) compound_values_YEN_GQH(
    time, coupon_rate, rate, redemption,
    coupon_frequency, compound_frequency
  )
  result <- newton_rate_YEN_GQH(
    function(rate) values(rate)$price,
    function(rate) values(rate)$dP_dY_pct,
    price, initial_rate, digits, max_iterations
  )
  truncate_decimal_YEN_GQH(result, digits)
}

#' Yen-bond risk measures from a simple rate
#'
#' @inheritParams bond_price_from_simple_rate_YEN_GQL
#' @param accrued_interest Accrued interest added to `price` for normalized
#'   duration and convexity.
#' @return A one-row tibble of analytic risk measures.
#' @export
bond_risk_from_simple_rate_YEN_GQL <- function(
    start_date, end_date, coupon_rate, simple_rate,
    redemption = 100, accrued_interest = 0) {
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  denominator <- 1 + simple_rate * time / 100
  numerator <- redemption + coupon_rate * time
  price <- numerator / denominator
  d1 <- -numerator * (time / 100) / denominator^2
  d2 <- 2 * numerator * (time / 100)^2 / denominator^3
  risk_tibble_YEN_GQH("simple_rate", simple_rate, price,
                      accrued_interest, d1, d2)
}

#' Yen-bond risk measures from a compound yield
#'
#' @inheritParams bond_price_from_compound_yield_YEN_GQL
#' @inheritParams bond_risk_from_simple_rate_YEN_GQL
#' @return A one-row tibble when tibble is installed, otherwise a data frame.
#' @export
bond_risk_from_compound_yield_YEN_GQL <- function(
    start_date, end_date, coupon_rate, compound_yield,
    redemption = 100, coupon_frequency = 2,
    compound_frequency = 2, accrued_interest = 0) {
  time <- bond_year_fraction_YEN_GQL(start_date, end_date)
  values <- compound_values_YEN_GQH(
    time, coupon_rate, compound_yield, redemption,
    coupon_frequency, compound_frequency
  )
  risk_tibble_YEN_GQH(
    "compound_yield", compound_yield, values$price,
    accrued_interest, values$dP_dY_pct, values$d2P_dY2_pct
  )
}

risk_tibble_YEN_GQH <- function(rate_name, rate, price, accrued_interest,
                                dp_dy_pct, d2p_dy_pct2) {
  dirty_price <- price + accrued_interest
  if (!is.finite(dirty_price) || dirty_price <= 0) {
    stop("Price plus accrued interest must be positive.", call. = FALSE)
  }

  tibble::tibble(
    rate_type = rate_name,
    rate_pct = rate,
    price = price,
    accrued_interest = accrued_interest,
    dirty_price = dirty_price,
    dP_dY_pct = dp_dy_pct,
    d2P_dY2_pct = d2p_dy_pct2,
    bpv_yen = -dp_dy_pct / 100,
    bpv_sen = -dp_dy_pct,
    modified_duration = -100 * dp_dy_pct / dirty_price,
    convexity = 10000 * d2p_dy_pct2 / dirty_price
  )
}

compound_values_YEN_GQH <- function(time, coupon_rate, compound_yield,
                                    redemption, coupon_frequency,
                                    compound_frequency) {
  base <- 1 + compound_yield / (100 * compound_frequency)
  if (!is.finite(base) || base <= 0) {
    stop("The compound yield is outside its valid domain.", call. = FALSE)
  }
  periods <- coupon_frequency * time
  ratio <- compound_frequency / coupon_frequency
  alpha <- base^(-ratio)

  if (compound_yield == 0) {
    price <- redemption + coupon_rate * time
    if (coupon_frequency == 2 && compound_frequency == 2) {
      d1 <- -(
        coupon_rate * time * (2 * time + 1) / 400 +
          redemption * time / 100
      )
      d2 <- time * (2 * time + 1) / 20000 *
        (coupon_rate * (time + 1) / 3 + redemption)
      return(list(
        price = price,
        dP_dY_pct = d1,
        d2P_dY2_pct = d2
      ))
    }
    p_alpha <- coupon_rate / coupon_frequency *
      periods * (periods + 1) / 2 + redemption * periods
    p_alpha2 <- coupon_rate / coupon_frequency *
      periods * (periods + 1) * (periods - 1) / 3 +
      redemption * periods * (periods - 1)
  } else {
    numerator <- alpha - alpha^(periods + 1)
    denominator <- 1 - alpha
    numerator_d1 <- 1 - (periods + 1) * alpha^periods
    numerator_d2 <- -periods * (periods + 1) * alpha^(periods - 1)
    quotient <- numerator_d1 * denominator + numerator
    series <- numerator / denominator
    series_d1 <- quotient / denominator^2
    series_d2 <- numerator_d2 / denominator + 2 * quotient / denominator^3
    price <- coupon_rate / coupon_frequency * series +
      redemption * alpha^periods
    p_alpha <- coupon_rate / coupon_frequency * series_d1 +
      redemption * periods * alpha^(periods - 1)
    p_alpha2 <- coupon_rate / coupon_frequency * series_d2 +
      redemption * periods * (periods - 1) * alpha^(periods - 2)
  }

  alpha_d1 <- -alpha / (100 * coupon_frequency * base)
  alpha_d2 <- alpha * ratio * (ratio + 1) /
    (10000 * compound_frequency^2 * base^2)
  list(
    price = price,
    dP_dY_pct = p_alpha * alpha_d1,
    d2P_dY2_pct = p_alpha2 * alpha_d1^2 + p_alpha * alpha_d2
  )
}

newton_rate_YEN_GQH <- function(price_function, derivative_function,
                                target_price, initial_rate, digits,
                                max_iterations) {
  epsilon <- 10^(-(digits + 1L))
  states <- purrr::accumulate(
    seq_len(max_iterations),
    .init = list(rate = initial_rate, converged = FALSE),
    .f = function(state, iteration) {
      if (state$converged) return(state)
      difference <- price_function(state$rate) - target_price
      derivative <- derivative_function(state$rate)
      if (!is.finite(derivative) || abs(derivative) <= .Machine$double.eps) {
        stop("Newton's method encountered an invalid derivative.", call. = FALSE)
      }
      next_rate <- state$rate - difference / derivative
      list(
        rate = next_rate,
        converged = is.finite(next_rate) && abs(difference) < epsilon
      )
    }
  )
  result <- purrr::detect(states, function(state) state$converged)
  if (is.null(result)) {
    stop("Newton's method did not converge.", call. = FALSE)
  }
  result$rate
}

validate_frequencies_YEN_GQH <- function(coupon_frequency, compound_frequency) {
  frequencies <- c(coupon_frequency, compound_frequency)
  if (any(!is.finite(frequencies)) || any(frequencies <= 0)) {
    stop("Coupon and compound frequencies must be positive.", call. = FALSE)
  }
  invisible(TRUE)
}

truncate_decimal_YEN_GQH <- function(value, digits) {
  if (is.null(digits)) return(value)
  if (length(digits) != 1L || is.na(digits) || digits < 0 || digits %% 1 != 0) {
    stop("`digits` must be NULL or a non-negative integer.", call. = FALSE)
  }
  scale <- 10^digits
  trunc(value * scale) / scale
}
