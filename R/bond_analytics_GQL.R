# bond_analytics.R


#' Create a QuantLib fixed-rate bond
#'
#' This is the main QuantLibGauss fixed-rate bond constructor.
#'
#' @param issue_date Issue date as an ISO character string.
#' @param maturity_date Maturity date as an ISO character string.
#' @param coupon_rate Coupon rate as a decimal.
#' @param face_amount Face amount / notional.
#' @param settlement_days Settlement days.
#' @param frequency QuantLib Period used for the coupon schedule.
#' @param calendar QuantLib calendar used for the schedule.
#' @param accrual_day_counter QuantLib day counter used for coupon accrual.
#' @param payment_convention Payment convention for the bond.
#' @param schedule_convention Schedule date adjustment convention.
#' @param maturity_convention Maturity date adjustment convention.
#' @param date_generation Date generation rule, for example "Backward".
#' @param end_of_month End-of-month schedule flag.
#' @param redemption Redemption amount.
#' @param schedule_frequency Optional alias for frequency.
#'
#' @return A QuantLib FixedRateBond object.
#' @export
fixed_rate_bond_GQL <- function(
  issue_date = "2007-05-15",
  maturity_date = "2017-05-15",
  coupon_rate = 0.045,
  face_amount = 100,
  settlement_days = 3,
  frequency = QuantLib::Period("Semiannual"),
  calendar = QuantLib::UnitedStates("GovernmentBond"),
  accrual_day_counter = QuantLib::ActualActual("Bond"),
  payment_convention = "ModifiedFollowing",
  schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted",
  date_generation = "Backward",
  end_of_month = FALSE,
  redemption = 100,
  schedule_frequency = frequency
) {
  issue_date_ql <- date_GQL(issue_date)
  maturity_date_ql <- date_GQL(maturity_date)

  if (is.character(date_generation)) {
    date_generation <- QuantLib::copyToR(
      QuantLib::DateGeneration(),
      date_generation
    )
  }

  schedule <- QuantLib::Schedule(
    issue_date_ql,
    maturity_date_ql,
    schedule_frequency,
    calendar,
    schedule_convention,
    maturity_convention,
    date_generation,
    end_of_month
  )

  QuantLib::FixedRateBond(
    settlement_days,
    face_amount,
    schedule,
    coupon_rate,
    accrual_day_counter,
    payment_convention,
    redemption,
    issue_date_ql
  )
}

#' Set Bond Pricing Engine
#'
#' @param bond QuantLib bond object.
#' @param curve QuantLib yield term structure.
#'
#' @return Bond with pricing engine.
#' @export
set_bond_pricing_engine_GQL <- function(bond, curve) {
  discounting_term_structure <- QuantLib::RelinkableYieldTermStructureHandle()

  invisible(
    QuantLib::RelinkableYieldTermStructureHandle_linkTo(
      discounting_term_structure,
      curve
    )
  )

  bond_engine <- QuantLib::DiscountingBondEngine(discounting_term_structure)

  invisible(
    QuantLib::Instrument_setPricingEngine(bond, bond_engine)
  )

  bond
}


#' Bond Yield
#' @export
bond_yield_GQL <- function(
  bond,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  QuantLib::Bond_yield(
    bond,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Duration
#' @export
bond_duration_GQL <- function(
  bond,
  ytm = NULL,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- bond_yield_GQL(bond, day_counter, compounding, frequency)
  }

  QuantLib::BondFunctions_duration(
    bond,
    ytm,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Convexity
#' @export
bond_convexity_GQL <- function(
  bond,
  ytm = NULL,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- bond_yield_GQL(bond, day_counter, compounding, frequency)
  }

  QuantLib::BondFunctions_convexity(
    bond,
    ytm,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Accrued Amount
#' @export
bond_accrued_GQL <- function(bond) {
  QuantLib::Bond_accruedAmount(bond)
}


#' Bond Example
#' @export
bond_example_GQL <- function() {

  eval_date_GQL("2010-01-01")
  curve <- build_bond_discount_curve_GQL()
  bond <- zero_coupon_bond_GQL(curve)

  list(
    bond = bond,
    yield = bond_yield_GQL(bond),
    duration = bond_duration_GQL(bond),
    convexity = bond_convexity_GQL(bond),
    summary = bond_summary_GQL(bond)
  )
}

#' Bond Price-Yield Curve
#'
#' @export
bond_price_yield_curve_GQL <- function(
  bond,
  ytm_center = NULL,
  spread = 0.02,
  n = 41,
  day_counter = QuantLib::Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm_center)) {
    ytm_center <- bond_yield_GQL(
      bond,
      day_counter,
      compounding,
      frequency
    )
  }

  ytm <- seq(
    from = ytm_center - spread,
    to = ytm_center + spread,
    length.out = n
  )

  price <- purrr::map_dbl(
    ytm,
    ~ bond_price_from_yield_GQL(
      bond,
      .x,
      day_counter,
      compounding,
      frequency
    )
  )

  tibble::tibble(
    yield = ytm,
    clean_price = price
  )
}
#' Bond Price from Yield
#'
#' @export
bond_price_from_yield_GQL <- function(
  bond,
  ytm,
  day_counter = QuantLib::Actual360(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  if (is.null(settlement_date)) {
    QuantLib::Bond_cleanPrice(
      bond,
      ytm,
      day_counter,
      compounding,
      frequency
    )
  } else {
    QuantLib::Bond_cleanPrice(
      bond,
      ytm,
      day_counter,
      compounding,
      frequency,
      date_GQL(settlement_date)
    )
  }
}

#' Bond Yield from Clean Price
#'
#' @export
bond_yield_from_price_GQL <- function(
  bond,
  clean_price,
  day_counter = QuantLib::Actual360(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  price <- QuantLib::BondPrice(
    clean_price,
    QuantLib::BondPrice_Clean_get()
  )

  if (is.null(settlement_date)) {
    QuantLib::Bond_yield(
      bond,
      price,
      day_counter,
      compounding,
      frequency
    )
  } else {
    QuantLib::Bond_yield(
      bond,
      price,
      day_counter,
      compounding,
      frequency,
      date_GQL(settlement_date)
    )
  }
}
#' Bond DV01
#'
#' Dollar value of 1bp yield change.
#'
#' @export
bond_dv01_GQL <- function(
  bond,
  ytm = NULL,
  bp = 1e-4,
  day_counter = QuantLib::Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- bond_yield_GQL(bond, day_counter, compounding, frequency)
  }

  price_down <- bond_price_from_yield_GQL(
    bond, ytm - bp, day_counter, compounding, frequency
  )

  price_up <- bond_price_from_yield_GQL(
    bond, ytm + bp, day_counter, compounding, frequency
  )

  (price_down - price_up) / 2
}

#' Bond PV01
#'
#' Present value change for 1bp yield change per face amount.
#'
#' @export
bond_pv01_GQL <- function(
  bond,
  ytm = NULL,
  bp = 1e-4,
  day_counter = QuantLib::Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  bond_dv01_GQL(
    bond,
    ytm,
    bp,
    day_counter,
    compounding,
    frequency
  )
}


#' Bond Cashflow Table
#'
#' @export
bond_cashflow_table_GQL <- function(bond) {
  leg_to_cashflow_tbl_GQL(QuantLib::Bond_cashflows(bond))
}

#' Create a zero coupon bond
#'
#' @export
zero_coupon_bond_GQL <- function(
    discount_curve,
    maturity = "2013-08-15",
    issue = "2008-08-15",
    face = 100
) {
  calendar <- QuantLib::UnitedStates("GovernmentBond")

  issueDate <- date_GQL(issue)

  maturityDate <- date_GQL(maturity)

  settlementDays <- 3

  redemption <- 100

  bond <- QuantLib::ZeroCouponBond(
    settlementDays,
    calendar,
    face,
    maturityDate,
    "Following",
    redemption,
    issueDate
  )

  engine <- QuantLib::DiscountingBondEngine(
    QuantLib::YieldTermStructureHandle(
      discount_curve
    )
  )

  QuantLib::Instrument_setPricingEngine(
    bond,
    engine
  )

  bond
}

#' Get bond coupon information
#'
#' @param bond A QuantLib bond object.
#' @param as_of Evaluation date. Default is  eval_date_get_GQL().
#'
#' @return A tibble with coupon cashflow information.
#'
#' @export
bond_coupon_info_GQL <- function(bond, as_of = eval_date_get_GQL()) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  cf <- bond_cashflow_table_GQL(bond)

  if (!"date" %in% names(cf)) {
    stop("bond_cashflow_table_GQL() must return a column named 'date'.")
  }

  if (!"amount" %in% names(cf)) {
    stop("bond_cashflow_table_GQL() must return a column named 'amount'.")
  }

  as_of <- as.Date(as_of)

  cf <- tibble::as_tibble(cf) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      amount = as.numeric(.data$amount),
      is_future = .data$date > as_of
    )

  # Redemption/principal を除き、coupon cashflow だけを推定する
  if ("type" %in% names(cf)) {
    coupons <- cf |>
      dplyr::filter(
        !grepl("redemption|principal|notional", .data$type, ignore.case = TRUE)
      )
  } else {
    # type 列がない場合は、最大金額を元本償還とみなして除外する簡易版
    max_amount <- max(abs(cf$amount), na.rm = TRUE)

    coupons <- cf |>
      dplyr::filter(abs(.data$amount) < max_amount)
  }

  coupons <- coupons |>
    dplyr::arrange(.data$date)

  previous_candidates <- coupons$date[coupons$date <= as_of]
  next_candidates <- coupons$date[coupons$date > as_of]

  previous_coupon_date <- if (length(previous_candidates) > 0) {
    max(previous_candidates)
  } else {
    as.Date(NA)
  }

  next_coupon_date <- if (length(next_candidates) > 0) {
    min(next_candidates)
  } else {
    as.Date(NA)
  }

  coupons |>
    dplyr::mutate(
      coupon_no = dplyr::row_number(),
      is_previous_coupon = !is.na(previous_coupon_date) &
        .data$date == previous_coupon_date,
      is_next_coupon = !is.na(next_coupon_date) &
        .data$date == next_coupon_date
    )
}
#' Get bond settlement information
#'
#' @export
#'
bond_settlement_info_GQL <- function(bond, as_of = eval_date_get_GQL()) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  as_of <- as.Date(as_of)

  coupon_info <- bond_coupon_info_GQL(bond, as_of = as_of)

  previous_coupon <- coupon_info |>
    dplyr::filter(.data$is_previous_coupon)

  next_coupon <- coupon_info |>
    dplyr::filter(.data$is_next_coupon)

  previous_coupon_date <- if (nrow(previous_coupon) > 0) {
    previous_coupon$date[[1]]
  } else {
    as.Date(NA)
  }

  next_coupon_date <- if (nrow(next_coupon) > 0) {
    next_coupon$date[[1]]
  } else {
    as.Date(NA)
  }

  days_since_previous_coupon <- if (!is.na(previous_coupon_date)) {
    as.integer(as_of - previous_coupon_date)
  } else {
    NA_integer_
  }

  days_to_next_coupon <- if (!is.na(next_coupon_date)) {
    as.integer(next_coupon_date - as_of)
  } else {
    NA_integer_
  }

  accrued_amount <- tryCatch(
    QuantLib::Bond_accruedAmount(bond),
    error = function(e) {
      tryCatch(
        bond$accruedAmount(),
        error = function(e2) NA_real_
      )
    }
  )

  settlement_date <- tryCatch(
    .ql_date_to_r_date_GQL(QuantLib::Bond_settlementDate(bond)),
    error = function(e) {
      tryCatch(
        .ql_date_to_r_date_GQL(bond$settlementDate()),
        error = function(e2) as.Date(NA)
      )
    }
  )

  settlement_days <- tryCatch(
    QuantLib::Bond_settlementDays(bond),
    error = function(e) {
      tryCatch(
        bond$settlementDays(),
        error = function(e2) NA_integer_
      )
    }
  )

  tibble::tibble(
    evaluation_date = as_of,
    settlement_date = settlement_date,
    settlement_days = as.integer(settlement_days),
    previous_coupon_date = previous_coupon_date,
    next_coupon_date = next_coupon_date,
    days_since_previous_coupon = days_since_previous_coupon,
    days_to_next_coupon = days_to_next_coupon,
    accrued_amount = as.numeric(accrued_amount)
  )
}

# Internal helper: convert QuantLib Date to R Date
.ql_date_to_r_date_GQL <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  tryCatch(
    as.Date(QuantLib::Date_ISO(x)),
    error = function(e1) {
      tryCatch(
        as.Date(x$ISO()),
        error = function(e2) as.Date(NA)
      )
    }
  )
}

#' Bond sensitivity table
#'
#' @param bond A QuantLib bond object.
#' @param ytm Base yield. If NULL, calculated from bond.
#' @param shifts_bp Yield shifts in basis points.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#'
#' @return A tibble with price sensitivity to yield shifts.
#'
#' @export
bond_sensitivity_table_GQL <- function(
  bond,
  ytm = NULL,
  shifts_bp = c(-100, -50, -25, -10, -1, 0, 1, 10, 25, 50, 100),
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  if (is.null(ytm)) {
    ytm <- bond_yield_GQL(
      bond,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    )
  }

  base_clean_price <- bond_price_from_yield_GQL(
    bond,
    ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  out <- tibble::tibble(
    shift_bp = shifts_bp,
    yield = ytm + shifts_bp / 10000
  ) |>
    dplyr::mutate(
      clean_price = purrr::map_dbl(
        .data$yield,
        ~ bond_price_from_yield_GQL(
          bond,
          .x,
          day_counter = day_counter,
          compounding = compounding,
          frequency = frequency
        )
      ),
      price_change = .data$clean_price - base_clean_price,
      price_change_pct = .data$price_change / base_clean_price
    )

  out
}


#' Bond Z-spread
#'
#' Calculate the constant spread over a discount curve that reproduces
#' the observed clean price of the bond.
#'
#' @param bond A QuantLib bond object.
#' @param discount_curve A QuantLib yield term structure.
#' @param clean_price Clean price. If NULL, QuantLib::Bond_cleanPrice(bond) is used.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#' @param settlement_date Optional settlement date as character or Date.
#'
#' @return Z-spread as a numeric value.
#'
#' @export
bond_zspread_GQL <- function(
  bond,
  discount_curve,
  clean_price = NULL,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  if (!exists("BondFunctions_zSpread", mode = "function")) {
    stop("QuantLib::BondFunctions_zSpread() is not available in this QuantLib build.")
  }

  if (is.null(clean_price)) {
    clean_price <- QuantLib::Bond_cleanPrice(bond)
  }

  price <- QuantLib::BondPrice(
    as.numeric(clean_price),
    QuantLib::BondPrice_Clean_get()
  )

  curve_handle <- QuantLib::YieldTermStructureHandle(discount_curve)

  if (is.null(settlement_date)) {
    out <- tryCatch(
      QuantLib::BondFunctions_zSpread(
        bond,
        price,
        curve_handle,
        day_counter,
        compounding,
        frequency
      ),
      error = function(e1) {
        tryCatch(
          QuantLib::BondFunctions_zSpread(
            bond,
            price,
            discount_curve,
            day_counter,
            compounding,
            frequency
          ),
          error = function(e2) {
            stop(
              "QuantLib::BondFunctions_zSpread() failed. Original error: ",
              conditionMessage(e1),
              "\nFallback error: ",
              conditionMessage(e2)
            )
          }
        )
      }
    )
  } else {
    settlement_date <- date_GQL(as.character(as.Date(settlement_date)))

    out <- tryCatch(
      QuantLib::BondFunctions_zSpread(
        bond,
        price,
        curve_handle,
        day_counter,
        compounding,
        frequency,
        settlement_date
      ),
      error = function(e1) {
        tryCatch(
          QuantLib::BondFunctions_zSpread(
            bond,
            price,
            discount_curve,
            day_counter,
            compounding,
            frequency,
            settlement_date
          ),
          error = function(e2) {
            stop(
              "QuantLib::BondFunctions_zSpread() with settlement_date failed. Original error: ",
              conditionMessage(e1),
              "\nFallback error: ",
              conditionMessage(e2)
            )
          }
        )
      }
    )
  }

  as.numeric(out)
}


#' Bond Summary
#'
#' @param bond A QuantLib bond object.
#' @param discount_curve Optional QuantLib yield term structure for Z-spread.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#'
#' @return A tibble with bond summary information.
#'
#' @export
bond_summary_GQL <- function(
  bond,
  discount_curve = NULL,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  requireNamespace("tibble", quietly = TRUE)

  ytm <- bond_yield_GQL(
    bond,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dur <- bond_duration_GQL(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  conv <- bond_convexity_GQL(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dv01 <- bond_dv01_GQL(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  pv01 <- bond_pv01_GQL(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  out <- tibble::tibble(
    item = c(
      "NPV",
      "Clean Price",
      "Dirty Price",
      "Accrued Amount",
      "Yield",
      "Yield (bp)",
      "Duration",
      "Convexity",
      "DV01",
      "PV01"
    ),
    value = c(
      QuantLib::Instrument_NPV(bond),
      QuantLib::Bond_cleanPrice(bond),
      QuantLib::Bond_dirtyPrice(bond),
      QuantLib::Bond_accruedAmount(bond),
      ytm,
      ytm * 10000,
      dur,
      conv,
      dv01,
      pv01
    )
  )

  if (!is.null(discount_curve) && exists("bond_zspread_GQL", mode = "function")) {
    z <- tryCatch(
      bond_zspread_GQL(
        bond = bond,
        discount_curve = discount_curve,
        clean_price = QuantLib::Bond_cleanPrice(bond),
        day_counter = day_counter,
        compounding = compounding,
        frequency = frequency
      ),
      error = function(e) NA_real_
    )

    out <- rbind(
      out,
      tibble::tibble(
        item = c("Z-spread", "Z-spread (bp)"),
        value = c(z, z * 10000)
      )
    )
  }

  out
}


#' Calculate futures BPV from CTD bond BPV
#'
#' Calculate futures BPV using a practical CTD-based approximation:
#'
#' \deqn{futures BPV = CTD bond BPV / conversion factor * contracts}
#'
#' @param ctd_bpv Numeric CTD bond BPV, DV01, or PV01.
#' @param conversion_factor Numeric futures conversion factor.
#' @param contracts Numeric number of futures contracts. Default is 1.
#'
#' @return Numeric futures BPV.
#' @export
futures_bpv_GQL <- function(ctd_bpv,
                            conversion_factor,
                            contracts = 1) {
  if (!is.numeric(ctd_bpv) || length(ctd_bpv) < 1L || anyNA(ctd_bpv)) {
    stop("ctd_bpv must be numeric and non-missing.", call. = FALSE)
  }

  if (!is.numeric(conversion_factor) ||
      length(conversion_factor) < 1L ||
      anyNA(conversion_factor) ||
      any(conversion_factor == 0)) {
    stop("conversion_factor must be numeric, non-missing, and non-zero.", call. = FALSE)
  }

  if (!is.numeric(contracts) || length(contracts) < 1L || anyNA(contracts)) {
    stop("contracts must be numeric and non-missing.", call. = FALSE)
  }

  ctd_bpv / conversion_factor * contracts
}

#' Calculate bond futures BPV from a CTD QuantLib bond
#'
#' Calculate futures BPV from a CTD QuantLib bond object by first calculating
#' the CTD bond sensitivity using either \code{bond_dv01_GQL()} or
#' \code{bond_pv01_GQL()}, then dividing it by the conversion factor.
#'
#' @param bond QuantLib bond object for the CTD bond.
#' @param conversion_factor Numeric futures conversion factor.
#' @param contracts Numeric number of futures contracts. Default is 1.
#' @param measure Sensitivity measure to use. Either \code{"dv01"} or \code{"pv01"}.
#' @param ytm Optional yield to maturity. If \code{NULL}, it is handled by
#'   \code{bond_dv01_GQL()} or \code{bond_pv01_GQL()}.
#' @param bp Basis point bump size. Default is 1e-04.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Coupon frequency.
#'
#' @return Numeric futures BPV.
#' @export
bond_futures_bpv_GQL <- function(bond,
                                 conversion_factor,
                                 contracts = 1,
                                 measure = c("dv01", "pv01"),
                                 ytm = NULL,
                                 bp = 1e-04,
                                 day_counter = QuantLib::Actual360(),
                                 compounding = "Compounded",
                                 frequency = "Annual") {
  measure <- match.arg(measure)

  ctd_bpv <- switch(
    measure,
    dv01 = bond_dv01_GQL(
      bond = bond,
      ytm = ytm,
      bp = bp,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    pv01 = bond_pv01_GQL(
      bond = bond,
      ytm = ytm,
      bp = bp,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    )
  )

  futures_bpv_GQL(
    ctd_bpv = ctd_bpv,
    conversion_factor = conversion_factor,
    contracts = contracts
  )
}

#' Bond yield from clean price
#'
#' Compatibility wrapper for examples that use an explicit clean-price name.
#'
#' @param bond QuantLib bond object.
#' @param clean_price Clean price.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#' @param settlement_date Optional settlement date.
#'
#' @return Numeric yield.
#' @export
bond_yield_from_clean_price_GQL <- function(
    bond,
    clean_price,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual",
    settlement_date = NULL
) {
  bond_yield_from_price_GQL(
    bond = bond,
    clean_price = clean_price,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency,
    settlement_date = settlement_date
  )
}

#' Bond price measures from yield
#'
#' @param bond QuantLib bond object.
#' @param yield Yield to maturity.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#' @param settlement_date Optional settlement date.
#' @param schedule Optional QuantLib schedule used to infer the previous coupon date.
#'
#' @return A tibble with price-related measures.
#' @export
bond_price_measures_GQL <- function(
    bond,
    yield,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual",
    settlement_date = NULL,
    schedule = NULL
) {
  accrued_amount <- tryCatch(
    bond_accrued_GQL(bond),
    error = function(e) tryCatch(bond$accruedAmount(), error = function(e) NA_real_)
  )

  clean_price <- tryCatch(
    bond_price_from_yield_GQL(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency,
      settlement_date = settlement_date
    ),
    error = function(e) tryCatch(
      bond$cleanPrice(yield, day_counter, compounding, frequency),
      error = function(e) NA_real_
    )
  )

  dirty_price <- tryCatch(
    bond$dirtyPrice(yield, day_counter, compounding, frequency),
    error = function(e) {
      if (is.na(clean_price) || is.na(accrued_amount)) {
        NA_real_
      } else {
        clean_price + accrued_amount
      }
    }
  )

  previous_coupon_date <- NULL
  accrued_days <- NA_real_

  if (!is.null(settlement_date) && !is.null(schedule)) {
    settlement_date_ql <- date_GQL(settlement_date)

    schedule_dates <- tryCatch(
      schedule_date_vector_GQL(schedule),
      error = function(e) NULL
    )

    if (!is.null(schedule_dates) && length(schedule_dates) > 0) {
      schedule_dates_as_date <- as.Date(
        purrr::map_chr(schedule_dates, iso_GQL)
      )

      settlement_as_date <- as.Date(iso_GQL(settlement_date_ql))
      prev_idx <- which(schedule_dates_as_date <= settlement_as_date)

      if (length(prev_idx) > 0) {
        previous_coupon_date <- schedule_dates[[max(prev_idx)]]
      }
    }

    if (!is.null(previous_coupon_date)) {
      accrued_days <- tryCatch(
        day_counter$dayCount(previous_coupon_date, settlement_date_ql),
        error = function(e) NA_real_
      )
    }
  }

  tibble::tibble(
    metric = c(
      "settlement_date",
      "previous_coupon_date",
      "accrued_days",
      "accrued_amount",
      "clean_price",
      "dirty_price"
    ),
    value = list(
      if (!is.null(settlement_date)) as.Date(iso_GQL(date_GQL(settlement_date))) else NA,
      if (!is.null(previous_coupon_date)) as.Date(iso_GQL(previous_coupon_date)) else NA,
      accrued_days,
      accrued_amount,
      clean_price,
      dirty_price
    )
  )
}

#' Bond risk measures from yield
#'
#' @param bond QuantLib bond object.
#' @param yield Yield to maturity.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#'
#' @return A tibble with duration, BPV/PV01, and convexity measures.
#' @export
bond_risk_measures_GQL <- function(
    bond,
    yield,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual"
) {
  modified_duration <- tryCatch(
    bond_duration_GQL(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) NA_real_
  )

  bpv <- tryCatch(
    bond_pv01_GQL(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) NA_real_
  )

  dv01 <- tryCatch(
    bond_dv01_GQL(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) NA_real_
  )

  convexity <- tryCatch(
    bond_convexity_GQL(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) NA_real_
  )

  tibble::tibble(
    metric = c(
      "modified_duration",
      "bpv",
      "dv01",
      "convexity"
    ),
    value = c(
      modified_duration,
      bpv,
      dv01,
      convexity
    )
  )
}

#' Build a flat-forward yield-curve handle for bond pricing
#'
#' @param settlement_date Settlement date.
#' @param rate Flat rate.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#'
#' @return QuantLib YieldTermStructureHandle.
#' @export
bond_flat_forward_handle_GQL <- function(
    settlement_date,
    rate,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual"
) {
  settlement_date <- date_GQL(settlement_date)

  curve_obj <- QuantLib::FlatForward(
    settlement_date,
    quote_handle_GQL(rate),
    day_counter,
    compounding,
    frequency
  )

  QuantLib::YieldTermStructureHandle(curve_obj)
}

#' Bond NPV with a curve handle
#'
#' @param bond QuantLib bond object.
#' @param curve_handle QuantLib YieldTermStructureHandle.
#'
#' @return Numeric NPV.
#' @export
bond_npv_with_curve_GQL <- function(
    bond,
    curve_handle
) {
  engine <- QuantLib::DiscountingBondEngine(curve_handle)
  bond$setPricingEngine(engine)

  tryCatch(
    bond$NPV(),
    error = function(e) QuantLib::Instrument_NPV(bond)
  )
}

#' Bond NPV with a flat yield
#'
#' @param bond QuantLib bond object.
#' @param settlement_date Settlement date.
#' @param rate Flat rate.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#'
#' @return Numeric NPV.
#' @export
bond_npv_with_flat_yield_GQL <- function(
    bond,
    settlement_date,
    rate,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual"
) {
  curve_handle <- bond_flat_forward_handle_GQL(
    settlement_date = settlement_date,
    rate = rate,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  bond_npv_with_curve_GQL(
    bond = bond,
    curve_handle = curve_handle
  )
}

#' Bond NPV with a z-spreaded curve
#'
#' @param bond QuantLib bond object.
#' @param base_curve_handle Base QuantLib YieldTermStructureHandle.
#' @param z_spread Z-spread.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#' @param day_counter QuantLib day counter.
#'
#' @return A list with spread curve, spread curve handle, and NPV.
#' @export
bond_npv_with_zspread_GQL <- function(
    bond,
    base_curve_handle,
    z_spread,
    compounding = "Compounded",
    frequency = "Annual",
    day_counter = QuantLib::Actual360()
) {
  spread_curve <- QuantLib::ZeroSpreadedTermStructure(
    base_curve_handle,
    quote_handle_GQL(z_spread),
    compounding,
    frequency,
    day_counter
  )

  spread_curve_handle <- QuantLib::YieldTermStructureHandle(spread_curve)

  list(
    spread_curve = spread_curve,
    spread_curve_handle = spread_curve_handle,
    npv = bond_npv_with_curve_GQL(
      bond = bond,
      curve_handle = spread_curve_handle
    )
  )
}

#' Hand-calculated bond risk measures
#'
#' Educational helper for checking BPV, duration, convexity, and a
#' delta-gamma price approximation from bumped dirty prices.
#'
#' @param bond QuantLib bond object.
#' @param yield Yield to maturity.
#' @param dirty_price Dirty price at the base yield.
#' @param modified_duration Modified duration.
#' @param convexity Convexity.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#'
#' @return A tibble with hand-calculated risk measures.
#' @export
bond_risk_handcalc_GQL <- function(
    bond,
    yield,
    dirty_price,
    modified_duration,
    convexity,
    day_counter = QuantLib::Actual360(),
    compounding = "Compounded",
    frequency = "Annual"
) {
  price_up_1bp <- tryCatch(
    bond$dirtyPrice(yield + 0.0001, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  price_down_1bp <- tryCatch(
    bond$dirtyPrice(yield - 0.0001, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  bpv_hand <- (price_up_1bp - price_down_1bp) / 2
  modified_duration_hand <- -bpv_hand * 100 / dirty_price

  convexity_hand <- (
    (price_up_1bp - dirty_price) -
      (dirty_price - price_down_1bp)
  ) * 10000 / dirty_price

  price_up_100bp <- tryCatch(
    bond$dirtyPrice(yield + 0.01, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  delta_approx <- -modified_duration / 100 * dirty_price
  gamma_approx <- convexity / 10000 * dirty_price
  price_approx_delta_gamma <- dirty_price + delta_approx + 0.5 * gamma_approx

  tibble::tibble(
    metric = c(
      "bpv_hand",
      "modified_duration_hand",
      "convexity_hand",
      "price_up_100bp",
      "price_approx_delta_gamma"
    ),
    value = c(
      bpv_hand,
      modified_duration_hand,
      convexity_hand,
      price_up_100bp,
      price_approx_delta_gamma
    )
  )
}

#' Build a US Treasury-style fixed-rate bond
#'
#' @param effective_date Issue/effective date.
#' @param maturity_date Maturity date.
#' @param coupon_rate_pct Coupon rate in percent.
#' @param face_amount Face amount.
#' @param settlement_days Settlement days.
#' @param calendar QuantLib calendar.
#' @param day_counter QuantLib day counter.
#'
#' @return A list containing the bond, schedule, and input metadata.
#' @export
us_treasury_bond_GQL <- function(
    effective_date,
    maturity_date,
    coupon_rate_pct,
    face_amount = 100,
    settlement_days = 1L,
    calendar = QuantLib::UnitedStates("GovernmentBond"),
    day_counter = QuantLib::ActualActual("Bond")
) {
  effective_date <- date_GQL(effective_date)
  maturity_date <- date_GQL(maturity_date)

  schedule <- QuantLib::Schedule(
    effective_date,
    maturity_date,
    period_months_GQL(6),
    calendar,
    "Unadjusted",
    "Unadjusted",
    "Backward",
    FALSE
  )

  bond <- QuantLib::FixedRateBond(
    as.integer(settlement_days),
    face_amount,
    schedule,
    c(coupon_rate_pct / 100),
    day_counter
  )

  list(
    bond = bond,
    schedule = schedule,
    effective_date = effective_date,
    maturity_date = maturity_date,
    coupon_rate_pct = coupon_rate_pct,
    face_amount = face_amount
  )
}

#' Calculate one-row gross basis measures for a deliverable Treasury bond
#'
#' @param issue_date Issue date.
#' @param maturity_date Maturity date.
#' @param coupon_rate_pct Coupon rate in percent.
#' @param conversion_factor Futures conversion factor.
#' @param market_yield_pct Market yield in percent.
#' @param settlement_date Settlement date.
#' @param futures_price Futures price.
#' @param settlement_days Settlement days.
#' @param calendar QuantLib calendar.
#' @param day_counter QuantLib day counter.
#' @param compounding QuantLib compounding convention or supported string.
#' @param frequency QuantLib frequency convention or supported string.
#'
#' @return A one-row tibble with gross basis inputs and measures.
#' @export
bond_futures_gross_basis_row_GQL <- function(
    issue_date,
    maturity_date,
    coupon_rate_pct,
    conversion_factor,
    market_yield_pct,
    settlement_date,
    futures_price,
    settlement_days = 1L,
    calendar = QuantLib::UnitedStates("GovernmentBond"),
    day_counter = QuantLib::ActualActual("Bond"),
    compounding = "Compounded",
    frequency = "Semiannual"
) {
  settlement_date_ql <- date_GQL(settlement_date)

  bond_bundle <- us_treasury_bond_GQL(
    effective_date = issue_date,
    maturity_date = maturity_date,
    coupon_rate_pct = coupon_rate_pct,
    settlement_days = settlement_days,
    calendar = calendar,
    day_counter = day_counter
  )

  bond_obj <- bond_bundle$bond
  ytm <- market_yield_pct / 100

  bpv_value <- tryCatch(
    bond_pv01_GQL(
      bond = bond_obj,
      ytm = ytm,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) NA_real_
  )

  clean_price <- tryCatch(
    bond_price_from_yield_GQL(
      bond = bond_obj,
      ytm = ytm,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency,
      settlement_date = settlement_date_ql
    ),
    error = function(e) tryCatch(
      bond_obj$cleanPrice(
        ytm,
        day_counter,
        compounding,
        frequency,
        settlement_date_ql
      ),
      error = function(e2) NA_real_
    )
  )

  dirty_price <- tryCatch(
    bond_obj$dirtyPrice(
      ytm,
      day_counter,
      compounding,
      frequency,
      settlement_date_ql
    ),
    error = function(e) tryCatch(
      bond_obj$dirtyPrice(ytm, day_counter, compounding, frequency),
      error = function(e2) {
        accrued <- tryCatch(
          bond_obj$accruedAmount(settlement_date_ql),
          error = function(e3) tryCatch(bond_obj$accruedAmount(), error = function(e4) NA_real_)
        )

        if (is.na(clean_price) || is.na(accrued)) {
          NA_real_
        } else {
          clean_price + accrued
        }
      }
    )
  )

  gross_basis <- clean_price - futures_price * conversion_factor

  tibble::tibble(
    issue_date = iso_GQL(bond_bundle$effective_date),
    maturity = iso_GQL(bond_obj$maturityDate()),
    coupon = tryCatch(bond_obj$nextCouponRate(), error = function(e) coupon_rate_pct / 100),
    yield_pct = market_yield_pct,
    bpv = bpv_value,
    clean_price = clean_price,
    dirty_price = dirty_price,
    conversion_factor = conversion_factor,
    gross_basis = gross_basis,
    bond_obj = list(bond_obj)
  )
}

#' Calculate one-row net basis, carry, and implied repo measures
#'
#' @param bond_obj QuantLib bond object.
#' @param conversion_factor Futures conversion factor.
#' @param clean_price Clean price.
#' @param dirty_price Dirty price.
#' @param gross_basis Gross basis.
#' @param settlement_date Settlement date.
#' @param repo_end_date Repo end date.
#' @param repo_rate Repo rate as a decimal.
#' @param repo_day_counter QuantLib day counter for repo accrual.
#' @param carry_day_counter QuantLib day counter for carry accrual.
#' @param futures_price Futures price.
#'
#' @return A one-row tibble with carry, net basis, and implied repo.
#' @export
bond_futures_net_basis_row_GQL <- function(
    bond_obj,
    conversion_factor,
    clean_price,
    dirty_price,
    gross_basis,
    settlement_date,
    repo_end_date,
    repo_rate,
    repo_day_counter = QuantLib::Actual360(),
    carry_day_counter = QuantLib::Actual360(),
    futures_price
) {
  settlement_date <- date_GQL(settlement_date)
  repo_end_date <- date_GQL(repo_end_date)

  accrued_start <- tryCatch(
    bond_obj$accruedAmount(settlement_date),
    error = function(e) tryCatch(bond_obj$accruedAmount(), error = function(e2) NA_real_)
  )

  accrued_end <- tryCatch(
    bond_obj$accruedAmount(repo_end_date),
    error = function(e) NA_real_
  )

  coupon_income <- accrued_end - accrued_start
  repo_year_fraction <- repo_day_counter$yearFraction(settlement_date, repo_end_date)
  repo_cost <- repo_rate * repo_year_fraction * dirty_price
  carry <- coupon_income - repo_cost
  net_basis <- gross_basis - carry
  forward_price <- clean_price - carry

  implied_repo <- if (is.na(repo_year_fraction) || repo_year_fraction == 0) {
    NA_real_
  } else {
    (
      (futures_price * conversion_factor + accrued_end) / dirty_price - 1
    ) / repo_year_fraction
  }

  tibble::tibble(
    accrued_start = accrued_start,
    accrued_end = accrued_end,
    coupon_income = coupon_income,
    dirty_price = dirty_price,
    repo_cost = repo_cost,
    carry = carry,
    net_basis = net_basis,
    forward_price = forward_price,
    implied_repo = implied_repo
  )
}
