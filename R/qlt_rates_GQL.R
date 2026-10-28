# qlt_rates_GQL.R
#
# Interest-rate curves, swaps, cashflow and fixing tables moved from QuantLib_tidy.

#' normalize curve data
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`normalize_curve_data()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_data Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
normalize_curve_data_GQL <- function(curve_data) {
  if (is.data.frame(curve_data)) {
    return(
      curve_data |>
        dplyr::transmute(
          kind = as.character(kind),
          tenor = as.character(tenor),
          rate_pct = as.numeric(rate_pct)
        )
    )
  }
  
  tibble::tibble(
    kind = purrr::map_chr(curve_data, ~ as.character(.x[[1]])),
    tenor = purrr::map_chr(curve_data, ~ as.character(.x[[2]])),
    rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
  )
}

#' make tibor curve
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`make_tibor_curve()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_data,trade_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_tibor_curve_GQL <- function(curve_data, trade_date = "2022-08-19") {
  curve_input_tbl <- normalize_curve_data_GQL(curve_data)
  
  trade_date_ql <- date_GQL(trade_date)
  set_eval_date_GQL(trade_date_ql)
  settle_date_ql <- date_GQL(advance_days_GQL(QuantLib::Japan(), trade_date_ql, 2))
  
  tibor_curve_handle <- QuantLib::RelinkableYieldTermStructureHandle()
  tibor_index <- QuantLib::Tibor(period_months_GQL(6), tibor_curve_handle)
  
  helper_list <- purrr::pmap(
    curve_input_tbl,
    function(kind, tenor, rate_pct) {
      tenor_obj <- switch(
        tenor,
        `6m` = period_months_GQL(6),
        `1y` = period_years_GQL(1),
        `18m` = period_months_GQL(18),
        `2y` = period_years_GQL(2),
        `3y` = period_years_GQL(3),
        `4y` = period_years_GQL(4),
        `5y` = period_years_GQL(5),
        `6y` = period_years_GQL(6),
        `7y` = period_years_GQL(7),
        stop("Unsupported tenor: ", tenor)
      )
      
      if (kind == "depo") {
        return(
          QuantLib::DepositRateHelper(
            quote_handle_GQL(rate_pct / 100),
            tibor_index
          )
        )
      }
      
      if (kind == "swap") {
        return(
          QuantLib::SwapRateHelper(
            quote_handle_GQL(rate_pct / 100),
            tenor_obj,
            QuantLib::Japan(),
            QuantLib::Frequency_Semiannual_get(),
            "ModifiedFollowing",
            QuantLib::Actual365Fixed(),
            tibor_index
          )
        )
      }
      
      stop("Unsupported helper type: ", kind)
    }
  )
  
  helper_vec <- push_rate_helpers_GQL(helper_list)
  
  tibor_curve_obj <- QuantLib::PiecewiseLogLinearDiscount(
    settle_date_ql,
    helper_vec,
    QuantLib::Actual365Fixed()
  )
  QuantLib::TermStructure_enableExtrapolation(tibor_curve_obj)
  QuantLib::RelinkableYieldTermStructureHandle_linkTo(tibor_curve_handle, tibor_curve_obj)
  
  list(
    trade_date = trade_date_ql,
    settle_date = settle_date_ql,
    tibor_index = tibor_index,
    curve = tibor_curve_obj,
    curve_handle = tibor_curve_handle,
    par_tbl = curve_input_tbl
  )
}

#' make schedule safe
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`make_schedule_safe()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param effective_date,maturity_date,tenor_obj,calendar_obj,convention,rule Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_schedule_safe_GQL <- function(effective_date, maturity_date, tenor_obj,
                               calendar_obj = QuantLib::Japan(),
                               convention = "ModifiedFollowing",
                               rule = "Backward") {
  QuantLib::Schedule(
    date_GQL(effective_date),
    date_GQL(maturity_date),
    tenor_obj,
    calendar_obj,
    convention,
    convention,
    rule,
    FALSE
  )
}

#' make vanilla tibor swap
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`make_vanilla_tibor_swap()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_handle,effective_date,maturity_date,nominal,fixed_rate,fixed_tenor,float_tenor,spread Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_vanilla_tibor_swap_GQL <- function(curve_handle,
                                    effective_date,
                                    maturity_date,
                                    nominal = 1e8,
                                    fixed_rate = 0.0020,
                                    fixed_tenor = period_months_GQL(6),
                                    float_tenor = period_months_GQL(6),
                                    spread = 0.0) {
  fixed_schedule <- make_schedule_safe_GQL(effective_date, maturity_date, fixed_tenor)
  float_schedule <- make_schedule_safe_GQL(effective_date, maturity_date, float_tenor)
  
  tibor_index <- QuantLib::Tibor(period_months_GQL(6), curve_handle)
  
  swap_obj <- QuantLib::VanillaSwap(
    QuantLib::Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    QuantLib::Actual365Fixed(),
    float_schedule,
    tibor_index,
    spread,
    QuantLib::Actual365Fixed()
  )
  
  engine_obj <- QuantLib::DiscountingSwapEngine(curve_handle)
  QuantLib::Instrument_setPricingEngine(swap_obj, engine_obj)
  
  list(
    swap = swap_obj,
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule,
    tibor_index = tibor_index,
    engine = engine_obj,
    effective_date = effective_date,
    maturity_date = maturity_date,
    spread = spread
  )
}

#' swap cashflow tbl
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`swap_cashflow_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param swap_obj,curve_obj,leg,day_count_obj Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
swap_cashflow_tbl_GQL <- function(swap_obj, curve_obj, leg = 1, day_count_obj = QuantLib::Actual365Fixed()) {
  target_leg <- swap_obj$leg(leg)
  
  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- leg_cashflow_at_GQH(target_leg, i)
    pay_date <- QuantLib::CashFlow_date(cashflow_obj)
    pay_date_chr <- iso_GQL(pay_date)
    pay_date_r <- as.Date(pay_date_chr)
    ref_date_r <- as.Date(iso_GQL(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365
    df <- if (t >= 0) curve_obj$discount(pay_date) else NA_real_
    
    if (leg == 1) {
      coupon_obj <- QuantLib::as_floating_rate_coupon(cashflow_obj)
      tibble::tibble(
        fixing_date = iso_GQL(QuantLib::FloatingRateCoupon_fixingDate(coupon_obj)),
        accrual_start = iso_GQL(QuantLib::Coupon_accrualStartDate(coupon_obj)),
        accrual_end = iso_GQL(QuantLib::Coupon_accrualEndDate(coupon_obj)),
        pay_date = pay_date_chr,
        days = day_count_obj$dayCount(QuantLib::Coupon_accrualStartDate(coupon_obj), QuantLib::Coupon_accrualEndDate(coupon_obj)),
        rate = coupon_obj$rate(),
        spread = QuantLib::FloatingRateCoupon_spread(coupon_obj),
        amount = QuantLib::CashFlow_amount(cashflow_obj),
        df = df,
        pv = QuantLib::CashFlow_amount(cashflow_obj) * df,
        leg = "floating"
      )
    } else {
      coupon_obj <- QuantLib::as_fixed_rate_coupon(cashflow_obj)
      tibble::tibble(
        fixing_date = NA_character_,
        accrual_start = iso_GQL(QuantLib::Coupon_accrualStartDate(coupon_obj)),
        accrual_end = iso_GQL(QuantLib::Coupon_accrualEndDate(coupon_obj)),
        pay_date = pay_date_chr,
        days = day_count_obj$dayCount(QuantLib::Coupon_accrualStartDate(coupon_obj), QuantLib::Coupon_accrualEndDate(coupon_obj)),
        rate = coupon_obj$rate(),
        spread = NA_real_,
        amount = QuantLib::CashFlow_amount(cashflow_obj),
        df = df,
        pv = QuantLib::CashFlow_amount(cashflow_obj) * df,
        leg = "fixed"
      )
    }
  })
}

#' coupon fixing status tbl
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base02.R` (`coupon_fixing_status_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param swap_obj,curve_obj,index_obj,leg,eval_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
coupon_fixing_status_tbl_GQL <- function(
    swap_obj,
    curve_obj,
    index_obj,
    leg = 1,
    eval_date = NULL
) {
  if (is.null(eval_date)) {
    eval_date <- QuantLib::Settings_instance()$evaluationDate()
  } else {
    eval_date <- date_GQL(eval_date)
  }
  
  target_leg <- swap_obj$leg(leg)
  
  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- leg_cashflow_at_GQH(target_leg, i)
    coupon_obj <- QuantLib::as_floating_rate_coupon(cashflow_obj)
    
    fixing_date_obj <- QuantLib::FloatingRateCoupon_fixingDate(coupon_obj)
    fixing_date_chr <- iso_GQL(fixing_date_obj)
    fixing_date_r <- as.Date(fixing_date_chr)
    eval_date_r <- as.Date(iso_GQL(eval_date))
    
    accrual_start_obj <- QuantLib::Coupon_accrualStartDate(coupon_obj)
    accrual_end_obj <- QuantLib::Coupon_accrualEndDate(coupon_obj)
    pay_date_obj <- QuantLib::CashFlow_date(cashflow_obj)
    pay_date_chr <- iso_GQL(pay_date_obj)
    
    pay_date_r <- as.Date(pay_date_chr)
    ref_date_r <- as.Date(iso_GQL(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365
    df <- if (t >= 0) curve_obj$discount(pay_date_obj) else NA_real_
    
    fixing_value <- tryCatch(
      index_obj$fixing(fixing_date_obj),
      error = function(e) NA_real_
    )
    
    coupon_spread <- tryCatch(
      QuantLib::FloatingRateCoupon_spread(coupon_obj),
      error = function(e) NA_real_
    )
    
    coupon_rate <- tryCatch(
      coupon_obj$rate(),
      error = function(e) NA_real_
    )
    
    amount_value <- tryCatch(
      QuantLib::CashFlow_amount(cashflow_obj),
      error = function(e) NA_real_
    )
    
    tibble::tibble(
      fixing_date = fixing_date_chr,
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      accrual_start = iso_GQL(accrual_start_obj),
      accrual_end = iso_GQL(accrual_end_obj),
      pay_date = pay_date_chr,
      amount = amount_value,
      coupon_rate = coupon_rate,
      spread = coupon_spread,
      fixing_value = fixing_value,
      implied_margin = coupon_rate - fixing_value,
      df = df,
      pv = amount_value * df,
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    )
  })
}

#' make tona curve
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base03.R` (`make_tona_curve()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_data,trade_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_tona_curve_GQL <- function(curve_data, trade_date = "2022-08-19") {
  .ois_curve_from_list_GQL(
    curve_data = curve_data,
    trade_date = trade_date,
    builder = build_tonar_curve_from_market_GQL,
    index_fun = tonar_GQL
  )
}

#' make ois swap
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base03.R` (`make_ois_swap()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param index_obj,curve_handle,effective_date,maturity_date,nominal,fixed_rate,spread,fixed_day_count,schedule_tenor,pay_lag,calendar_obj,convention Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_ois_swap_GQL <- function(index_obj,
                          curve_handle,
                          effective_date,
                          maturity_date,
                          nominal = 1e7,
                          fixed_rate = 0.05,
                          spread = 0.0,
                          fixed_day_count = QuantLib::Actual365Fixed(),
                          schedule_tenor = period_years_GQL(1),
                          pay_lag = 2,
                          calendar_obj = QuantLib::Japan(),
                          convention = "ModifiedFollowing") {
  fixed_schedule <- QuantLib::Schedule(
    date_GQL(effective_date),
    date_GQL(maturity_date),
    schedule_tenor,
    calendar_obj,
    convention,
    convention,
    "Backward",
    FALSE
  )
  
  swap_obj <- QuantLib::OvernightIndexedSwap(
    QuantLib::Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_count,
    index_obj,
    spread,
    pay_lag
  )
  
  engine_obj <- QuantLib::DiscountingSwapEngine(curve_handle)
  QuantLib::Instrument_setPricingEngine(swap_obj, engine_obj)
  
  list(
    swap = swap_obj,
    fixed_schedule = fixed_schedule,
    effective_date = effective_date,
    maturity_date = maturity_date,
    pay_lag = pay_lag,
    spread = spread,
    fixed_rate = fixed_rate,
    index = index_obj,
    engine = engine_obj
  )
}

#' tona daily fixing tbl
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base03.R` (`tona_daily_fixing_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param accrual_start,accrual_end,index_obj,eval_date,calendar_obj Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
tona_daily_fixing_tbl_GQL <- function(
    accrual_start,
    accrual_end,
    index_obj,
    eval_date,
    calendar_obj = QuantLib::Japan()
) {
  eval_date_r <- as.Date(eval_date)
  
  business_dates_tbl_GQL(
    start_date = accrual_start,
    end_date   = accrual_end,
    calendar_obj = calendar_obj,
    include_end = FALSE
  ) |>
    dplyr::mutate(
      fixing_date = accrual_date,
      fixing_date_r = as.Date(fixing_date),
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      fixing_value = purrr::map_dbl(
        fixing_date,
        ~ tryCatch(index_obj$fixing(date_GQL(.x)), error = function(e) NA_real_)
      ),
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    ) |>
    dplyr::select(
      accrual_date,
      fixing_date,
      fixing_before_eval,
      fixing_on_eval,
      fixing_value,
      fixing_source
    )
}

#' make sofr curve
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base03.R` (`make_sofr_curve()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_data,trade_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_sofr_curve_GQL <- function(curve_data, trade_date = "2023-09-26") {
  .ois_curve_from_list_GQL(
    curve_data = curve_data,
    trade_date = trade_date,
    builder = build_sofr_curve_from_market_GQL,
    index_fun = sofr_GQL
  )
}

# Convert list(c(kind, tenor, rate_pct), ...) quotes, build the curve with the
# market-quote builder (log-linear discount, deposit + spot OIS helpers), and
# return the bundle used by the QuantLib_tidy scripts.
.ois_curve_from_list_GQL <- function(curve_data, trade_date, builder, index_fun) {
  quote_tbl <- tibble::tibble(
    kind = purrr::map_chr(curve_data, 1),
    tenor = purrr::map_chr(curve_data, 2),
    rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
  )

  tenor_units <- c(d = "Days", w = "Weeks", m = "Months", y = "Years")

  bundle <- builder(
    quotes = quote_tbl |>
      dplyr::transmute(
        quote_id = paste(kind, tenor, sep = "_"),
        instrument_type = dplyr::recode(kind, depo = "deposit", swap = "spot_ois"),
        rate = rate_pct / 100,
        fixing_days = 2L,
        tenor_n = as.integer(sub("[A-Za-z]+$", "", tenor)),
        tenor_unit = unname(tenor_units[tolower(sub("^[0-9]+", "", tenor))])
      ),
    evaluation_date = trade_date,
    curve_type = "log_linear_discount"
  )

  curve_handle <- QuantLib::RelinkableYieldTermStructureHandle()
  QuantLib::RelinkableYieldTermStructureHandle_linkTo(curve_handle, bundle$curve)

  list(
    trade_date = date_GQL(trade_date),
    settle_date = bundle$curve$referenceDate(),
    index = index_fun(curve_handle),
    curve = bundle$curve,
    curve_handle = curve_handle,
    quote_tbl = quote_tbl
  )
}

#' make term sofr basis curve
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base03.R` (`make_term_sofr_basis_curve()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param sofr_bundle,term_sofr_3m_rate,basis_data Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_term_sofr_basis_curve_GQL <- function(sofr_bundle,
                                       term_sofr_3m_rate = 5.38558,
                                       basis_data = list(
                                         c("6m", 0.0),
                                         c("1y", 0.0),
                                         c("2y", 0.0),
                                         c("3y", 0.0)
                                       )) {
  term_curve_handle <- QuantLib::RelinkableYieldTermStructureHandle()
  term_sofr_index <- QuantLib::IborIndex(
    "TermSofr",
    period_months_GQL(3),
    2,
    QuantLib::USDCurrency(),
    sofr_bundle$index$fixingCalendar(),
    "ModifiedFollowing",
    TRUE,
    QuantLib::Actual360(),
    term_curve_handle
  )
  
  helper_list <- list(
    QuantLib::DepositRateHelper(
      quote_handle_GQL(term_sofr_3m_rate / 100),
      term_sofr_index
    )
  )
  
  basis_helpers <- purrr::map(basis_data, function(x) {
    tenor <- as.character(x[[1]])
    basis_bp <- as.numeric(x[[2]])
    
    QuantLib::OvernightIborBasisSwapRateHelper(
      quote_handle_GQL(basis_bp / 100),
      period_GQL(tenor),
      2,
      sofr_bundle$index$fixingCalendar(),
      "ModifiedFollowing",
      FALSE,
      sofr_bundle$index,
      term_sofr_index,
      sofr_bundle$curve_handle
    )
  })
  
  helper_vec <- push_rate_helpers_GQL(c(helper_list, basis_helpers))
  
  term_curve_obj <- QuantLib::PiecewiseLogLinearDiscount(
    2,
    sofr_bundle$index$fixingCalendar(),
    helper_vec,
    QuantLib::Actual360()
  )
  QuantLib::TermStructure_enableExtrapolation(term_curve_obj)
  QuantLib::RelinkableYieldTermStructureHandle_linkTo(term_curve_handle, term_curve_obj)
  
  list(
    index = term_sofr_index,
    curve = term_curve_obj,
    curve_handle = term_curve_handle
  )
}

#' cashflow leg tbl
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base07.R` (`cashflow_leg_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param leg_obj,curve_obj Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
cashflow_leg_tbl_GQL <- function(leg_obj, curve_obj = NULL) {
  ref_date_r <- if (!is.null(curve_obj)) as.Date(iso_GQL(curve_obj$referenceDate())) else NA
  
  purrr::map_dfr(seq_len(leg_obj$size()), function(i) {
    cf_obj <- leg_cashflow_at_GQH(leg_obj, i)
    pay_date_obj <- QuantLib::CashFlow_date(cf_obj)
    pay_date_chr <- iso_GQL(pay_date_obj)
    pay_date_r <- as.Date(pay_date_chr)
    amount_value <- tryCatch(QuantLib::CashFlow_amount(cf_obj), error = function(e) NA_real_)
    
    df_value <- if (!is.null(curve_obj)) {
      tryCatch(curve_obj$discount(pay_date_obj), error = function(e) NA_real_)
    } else {
      NA_real_
    }
    
    coupon_obj <- tryCatch(QuantLib::as_coupon(cf_obj), error = function(e) NULL)
    
    tibble::tibble(
      pay_date = pay_date_chr,
      accrual_start = if (!is.null(coupon_obj)) iso_GQL(QuantLib::Coupon_accrualStartDate(coupon_obj)) else NA_character_,
      accrual_end = if (!is.null(coupon_obj)) iso_GQL(QuantLib::Coupon_accrualEndDate(coupon_obj)) else NA_character_,
      amount = amount_value,
      rate = if (!is.null(coupon_obj)) tryCatch(coupon_obj$rate(), error = function(e) NA_real_) else NA_real_,
      df = df_value,
      pv = amount_value * df_value
    )
  })
}

#' fixed leg detail tbl
#'
#' Moved from `QuantLib_tidy/R/swaps/tidy_cashflow.R` (`fixed_leg_detail_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param leg,eval_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
fixed_leg_detail_tbl_GQL <- function(leg, eval_date) {
  tibble::tibble(
    idx = seq_len(leg$size())
  ) |>
    dplyr::mutate(
      cashflow = purrr::map(
        idx,
        function(i) leg[i][[1]]
      ),
      coupon = purrr::map(
        cashflow,
        function(cf) QuantLib::as_coupon(cf)
      ),
      fixed_coupon = purrr::map(
        cashflow,
        function(cf) QuantLib::as_fixed_rate_coupon(cf)
      ),
      ir = purrr::map(
        fixed_coupon,
        function(fc) QuantLib::FixedRateCoupon_interestRate(fc)
      ),
      date = purrr::map_chr(
        cashflow,
        function(cf) QuantLib::Date_ISO(QuantLib::CashFlow_date(cf))
      ),
      amount = purrr::map_dbl(
        cashflow,
        function(cf) QuantLib::CashFlow_amount(cf)
      ),
      accrualStartDate = purrr::map_chr(
        coupon,
        function(cp) QuantLib::Date_ISO(QuantLib::Coupon_accrualStartDate(cp))
      ),
      accrualEndDate = purrr::map_chr(
        coupon,
        function(cp) QuantLib::Date_ISO(QuantLib::Coupon_accrualEndDate(cp))
      ),
      accrualPeriod = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accrualPeriod(cp)
      ),
      accrualDays = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accrualDays(cp)
      ),
      accrualDayCounter = purrr::map_chr(
        coupon,
        function(cp) QuantLib::DayCounter_name(QuantLib::Coupon_dayCounter(cp))
      ),
      accruedAmount = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accruedAmount(cp, eval_date)
      ),
      rate = purrr::map_dbl(
        ir,
        function(x) QuantLib::InterestRate_rate(x)
      ),
      rateDayCount = purrr::map_chr(
        ir,
        function(x) QuantLib::DayCounter_name(QuantLib::InterestRate_dayCounter(x))
      )
    ) |>
    dplyr::select(
      date,
      amount,
      accrualStartDate,
      accrualEndDate,
      accrualPeriod,
      accrualDays,
      accrualDayCounter,
      accruedAmount,
      rate,
      rateDayCount
    )
}

#' floating leg detail tbl
#'
#' Moved from `QuantLib_tidy/R/swaps/tidy_cashflow.R` (`floating_leg_detail_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param leg,eval_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
floating_leg_detail_tbl_GQL <- function(leg, eval_date) {
  tibble::tibble(
    idx = seq_len(leg$size())
  ) |>
    dplyr::mutate(
      cashflow = purrr::map(
        idx,
        function(i) leg[i][[1]]
      ),
      coupon = purrr::map(
        cashflow,
        function(cf) QuantLib::as_coupon(cf)
      ),
      float_coupon = purrr::map(
        cashflow,
        function(cf) QuantLib::as_floating_rate_coupon(cf)
      ),
      date = purrr::map_chr(
        cashflow,
        function(cf) QuantLib::Date_ISO(QuantLib::CashFlow_date(cf))
      ),
      amount = purrr::map_dbl(
        cashflow,
        function(cf) QuantLib::CashFlow_amount(cf)
      ),
      accrualStartDate = purrr::map_chr(
        coupon,
        function(cp) QuantLib::Date_ISO(QuantLib::Coupon_accrualStartDate(cp))
      ),
      accrualEndDate = purrr::map_chr(
        coupon,
        function(cp) QuantLib::Date_ISO(QuantLib::Coupon_accrualEndDate(cp))
      ),
      accrualPeriod = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accrualPeriod(cp)
      ),
      accrualDays = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accrualDays(cp)
      ),
      accrualDayCounter = purrr::map_chr(
        coupon,
        function(cp) QuantLib::DayCounter_name(QuantLib::Coupon_dayCounter(cp))
      ),
      accruedAmount = purrr::map_dbl(
        coupon,
        function(cp) QuantLib::Coupon_accruedAmount(cp, eval_date)
      ),
      spread = purrr::map_dbl(
        float_coupon,
        function(fc) QuantLib::FloatingRateCoupon_spread(fc)
      ),
      gearing = purrr::map_dbl(
        float_coupon,
        function(fc) QuantLib::FloatingRateCoupon_gearing(fc)
      ),
      adjFix = purrr::map_dbl(
        float_coupon,
        function(fc) QuantLib::FloatingRateCoupon_adjustedFixing(fc)
      ),
      dtFix = purrr::map_chr(
        float_coupon,
        function(fc) QuantLib::Date_ISO(QuantLib::FloatingRateCoupon_fixingDate(fc))
      ),
      convexityAdj = purrr::map_dbl(
        float_coupon,
        function(fc) QuantLib::FloatingRateCoupon_convexityAdjustment(fc)
      )
    ) |>
    dplyr::select(
      date,
      amount,
      accrualStartDate,
      accrualEndDate,
      accrualPeriod,
      accrualDays,
      accrualDayCounter,
      accruedAmount,
      spread,
      gearing,
      adjFix,
      dtFix,
      convexityAdj
    )
}

#' curve pillar report tbl
#'
#' Moved from `QuantLib_tidy/R/swaps/tisy_swap.R` (`print_curve_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve,helpers Arguments of the original helper.
#' @param day_counter QuantLib day counter, or a list with one per helper.
#' @param compounding Compounding name, or one per helper.
#' @param frequency Frequency name used for the zero rates.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
curve_pillar_report_tbl_GQL <- function(
  curve,
  helpers,
  day_counter = QuantLib::Actual365Fixed(),
  compounding = "Continuous",
  frequency = "Annual"
) {
  n_helpers <- helpers$size()
  day_counters <- if (is.list(day_counter)) day_counter else list(day_counter)
  day_counters <- rep_len(day_counters, n_helpers)
  compoundings <- rep_len(as.character(compounding), n_helpers)
  
  tibble::tibble(idx = seq_len(n_helpers)) |>
    dplyr::mutate(
      helper = purrr::map(
        idx,
        function(i) helpers[i][[1]]
      ),
      pillar = purrr::map(
        helper,
        function(h) QuantLib::RateHelper_pillarDate(h)
      ),
      pillar_chr = purrr::map_chr(
        pillar,
        function(d) QuantLib::Date_ISO(d)
      ),
      zero_rate = purrr::pmap_dbl(
        list(pillar, day_counters, compoundings),
        function(d, dc, comp) {
          r <- QuantLib::YieldTermStructure_zeroRate(
            curve,
            d,
            dc,
            comp,
            frequency
          )
          QuantLib::InterestRate_rate(r) * 100.0
        }
      ),
      df = purrr::map_dbl(
        pillar,
        function(d) QuantLib::YieldTermStructure_discount(curve, d)
      ),
      implied_rate = purrr::map_dbl(
        helper,
        function(h) QuantLib::RateHelper_impliedQuote(h)
      )
    ) |>
    dplyr::select(
      pillar = pillar_chr,
      zeroRate = zero_rate,
      df,
      impliedRate = implied_rate
    )
}
