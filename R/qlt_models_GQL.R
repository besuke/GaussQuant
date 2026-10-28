# qlt_models_GQL.R
#
# Short-rate, Gaussian1d, option, basket and credit helpers moved from QuantLib_tidy.

#' safe discount bond
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base06.R` (`safe_discount_bond()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param model_obj,t0,t1,short_rate Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
safe_discount_bond_GQL <- function(model_obj, t0, t1, short_rate = 0.03) {
  if (is.null(model_obj)) return(NA_real_)
  
  out <- tryCatch(model_obj$discountBond(t0, t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(model_obj$discountBond(t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  NA_real_
}

#' simulate vasicek path
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base06.R` (`simulate_vasicek_path()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param n_steps,dt,r0,kappa,theta,sigma,seed Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
simulate_vasicek_path_GQL <- function(n_steps = 120,
                                  dt = 1 / 12,
                                  r0 = 0.03,
                                  kappa = 0.15,
                                  theta = 0.03,
                                  sigma = 0.01,
                                  seed = 123) {
  set.seed(seed)
  
  z_vec <- rnorm(n_steps)
  r_vec <- numeric(n_steps + 1)
  r_vec[1] <- r0
  
  for (i in seq_len(n_steps)) {
    r_vec[i + 1] <- r_vec[i] +
      kappa * (theta - r_vec[i]) * dt +
      sigma * sqrt(dt) * z_vec[i]
  }
  
  tibble::tibble(
    step = 0:n_steps,
    time = (0:n_steps) * dt,
    short_rate = r_vec
  )
}

#' simulate cir path
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base06.R` (`simulate_cir_path()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param n_steps,dt,r0,kappa,theta,sigma,seed Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
simulate_cir_path_GQL <- function(n_steps = 120,
                              dt = 1 / 12,
                              r0 = 0.03,
                              kappa = 0.15,
                              theta = 0.03,
                              sigma = 0.02,
                              seed = 123) {
  set.seed(seed)
  
  z_vec <- rnorm(n_steps)
  r_vec <- numeric(n_steps + 1)
  r_vec[1] <- r0
  
  for (i in seq_len(n_steps)) {
    next_r <- r_vec[i] +
      kappa * (theta - r_vec[i]) * dt +
      sigma * sqrt(pmax(r_vec[i], 0)) * sqrt(dt) * z_vec[i]
    r_vec[i + 1] <- pmax(next_r, 0)
  }
  
  tibble::tibble(
    step = 0:n_steps,
    time = (0:n_steps) * dt,
    short_rate = r_vec
  )
}

#' make hw swaption setup
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base06.R` (`make_hw_swaption_setup()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param curve_handle,exercise_date,swap_start,swap_maturity,nominal,fixed_rate Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_hw_swaption_setup_GQL <- function(curve_handle,
                                   exercise_date = "2024-01-03",
                                   swap_start = "2024-01-05",
                                   swap_maturity = "2029-01-05",
                                   nominal = 1e6,
                                   fixed_rate = 0.03) {
  fixed_schedule <- QuantLib::Schedule(
    date_GQL(swap_start),
    date_GQL(swap_maturity),
    period_years_GQL(1),
    QuantLib::TARGET(),
    "ModifiedFollowing",
    "ModifiedFollowing",
    "Backward",
    FALSE
  )
  
  float_schedule <- QuantLib::Schedule(
    date_GQL(swap_start),
    date_GQL(swap_maturity),
    period_months_GQL(6),
    QuantLib::TARGET(),
    "ModifiedFollowing",
    "ModifiedFollowing",
    "Backward",
    FALSE
  )
  
  euribor6m_index <- QuantLib::Euribor6M(curve_handle)
  
  swap_obj <- QuantLib::VanillaSwap(
    QuantLib::Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    QuantLib::Thirty360("BondBasis"),
    float_schedule,
    euribor6m_index,
    0.0,
    QuantLib::Actual360()
  )
  
  exercise_obj <- QuantLib::EuropeanExercise(date_GQL(exercise_date))
  swaption_obj <- QuantLib::Swaption(swap_obj, exercise_obj)
  
  list(
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule,
    index = euribor6m_index,
    swap = swap_obj,
    exercise = exercise_obj,
    swaption = swaption_obj
  )
}

#' survival tbl
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base08.R` (`survival_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param default_curve_obj,n Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
survival_tbl_GQL <- function(default_curve_obj, n = 200) {
  max_t <- tryCatch(default_curve_obj$maxTime(), error = function(e) 10)
  ref_date_r <- tryCatch(as.Date(iso_GQL(default_curve_obj$referenceDate())), error = function(e) as.Date("2023-01-03"))
  times <- seq(0, max_t, length.out = n)
  
  tibble::tibble(time = times) |>
    dplyr::mutate(
      survival_probability = purrr::map_dbl(
        time,
        ~ tryCatch(default_curve_obj$survivalProbability(.x), error = function(e) NA_real_)
      ),
      hazard_rate_proxy = dplyr::if_else(time > 0 & !is.na(survival_probability), -log(survival_probability) / time, 0),
      curve_date = ref_date_r + round(time * 365)
    )
}

#' price european call scenario
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base09.R` (`price_european_call_scenario()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param spot,volatility,eval_date,calendar_obj,day_counter_obj,dividend_handle,risk_free_handle,payoff_call,european_exercise Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
price_european_call_scenario_GQL <- function(spot, volatility, eval_date, calendar_obj, day_counter_obj, dividend_handle, risk_free_handle, payoff_call, european_exercise) {
  local_spot_quote <- QuantLib::SimpleQuote(spot)
  local_spot_handle <- QuantLib::QuoteHandle(local_spot_quote)
  
  local_vol_curve <- QuantLib::BlackConstantVol(eval_date, calendar_obj, volatility, day_counter_obj)
  QuantLib::TermStructure_enableExtrapolation(local_vol_curve)
  local_vol_handle <- QuantLib::BlackVolTermStructureHandle(local_vol_curve)
  
  local_process <- QuantLib::BlackScholesMertonProcess(
    local_spot_handle,
    dividend_handle,
    risk_free_handle,
    local_vol_handle
  )
  
  local_engine <- tryCatch(QuantLib::AnalyticEuropeanEngine(local_process), error = function(e) NULL)
  if (is.null(local_engine)) return(NA_real_)
  
  local_option <- QuantLib::VanillaOption(payoff_call, european_exercise)
  ok <- instrument_set_engine_safe_GQL(local_option, local_engine)
  if (!ok) return(NA_real_)
  
  instrument_npv_safe_GQL(local_option)
}

#' price fx call scenario
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base10.R` (`price_fx_call_scenario()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param spot,volatility,domestic_rate_local,eval_date,calendar_obj,day_counter_obj,foreign_rate,payoff_call,exercise_obj Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
price_fx_call_scenario_GQL <- function(spot, volatility, domestic_rate_local, eval_date, calendar_obj, day_counter_obj, foreign_rate, payoff_call, exercise_obj) {
  local_spot_quote <- QuantLib::SimpleQuote(spot)
  local_spot_handle <- QuantLib::QuoteHandle(local_spot_quote)
  
  local_domestic_curve <- QuantLib::FlatForward(eval_date, domestic_rate_local, day_counter_obj)
  local_foreign_curve <- QuantLib::FlatForward(eval_date, foreign_rate, day_counter_obj)
  local_vol_curve <- QuantLib::BlackConstantVol(eval_date, calendar_obj, volatility, day_counter_obj)
  
  QuantLib::TermStructure_enableExtrapolation(local_domestic_curve)
  QuantLib::TermStructure_enableExtrapolation(local_foreign_curve)
  
  local_domestic_handle <- QuantLib::YieldTermStructureHandle(local_domestic_curve)
  local_foreign_handle <- QuantLib::YieldTermStructureHandle(local_foreign_curve)
  local_vol_handle <- QuantLib::BlackVolTermStructureHandle(local_vol_curve)
  
  local_process <- QuantLib::BlackScholesMertonProcess(
    local_spot_handle,
    local_foreign_handle,
    local_domestic_handle,
    local_vol_handle
  )
  
  local_engine <- tryCatch(QuantLib::AnalyticEuropeanEngine(local_process), error = function(e) NULL)
  if (is.null(local_engine)) return(NA_real_)
  
  local_option <- QuantLib::VanillaOption(payoff_call, exercise_obj)
  ok <- instrument_set_engine_safe_GQL(local_option, local_engine)
  if (!ok) return(NA_real_)
  
  instrument_npv_safe_GQL(local_option)
}

#' append quote handles
#'
#' Moved from `QuantLib_tidy/R/options/tidy_gaussina1d.R` (`append_quote_handles()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param qh_list Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
append_quote_handles_GQL <- function(qh_list) {
  vec <- QuantLib::QuoteHandleVector()
  purrr::walk(
    qh_list,
    function(qh) {
      QuantLib::QuoteHandleVector_append(vec, qh)
    }
  )
  vec
}

#' shifted exercise dates
#'
#' Moved from `QuantLib_tidy/R/options/tidy_gaussina1d.R` (`schedule_dates_to_shifted_exercise_dates()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param schedule_obj,calendar,lag_days Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
shifted_exercise_dates_GQL <- function(schedule_obj, calendar, lag_days = 2) {
  exercise_dates <- QuantLib::Schedule_dates(self = schedule_obj, .copy = TRUE)
  
  for (i in seq(0, exercise_dates$size() - 1)) {
    shifted_date <- QuantLib::Calendar_advance(
      calendar,
      QuantLib::DateVector___getitem__(exercise_dates, i),
      -lag_days,
      "Days"
    )
    QuantLib::DateVector___setitem__(exercise_dates, i, shifted_date)
  }
  
  QuantLib::DateVector___delitem__(exercise_dates, 0)
  QuantLib::DateVector___delitem__(exercise_dates, QuantLib::DateVector___len__(exercise_dates) - 1)
  
  exercise_dates
}

#' set swaption engines
#'
#' Moved from `QuantLib_tidy/R/options/tidy_gaussina1d.R` (`set_swaption_engines()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param basket,engine Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
set_swaption_engines_GQL <- function(basket, engine) {
  purrr::walk(
    seq_len(basket$size()),
    function(i) {
      basket_i <- basket[i][[1]]
      QuantLib::BlackCalibrationHelper_setPricingEngine(basket_i, engine)
    }
  )
  invisible(NULL)
}

#' swaption basket tbl
#'
#' Moved from `QuantLib_tidy/R/options/tidy_gaussina1d.R` (`basket_data_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param basket Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
swaption_basket_tbl_GQL <- function(basket) {
  tibble::tibble(idx = seq_len(basket$size())) |>
    dplyr::mutate(
      basket_item = purrr::map(
        idx,
        function(i) basket[i][[1]]
      ),
      helper = purrr::map(
        basket_item,
        function(x) QuantLib::as_swaption_helper(x)
      ),
      swaption_obj = purrr::map(
        helper,
        function(h) QuantLib::SwaptionHelper_swaption(h)
      ),
      expiry = purrr::map_chr(
        helper,
        function(h) QuantLib::Date_ISO(QuantLib::SwaptionHelper_swaptionExpiryDate(h))
      ),
      maturity = purrr::map_chr(
        helper,
        function(h) QuantLib::Date_ISO(QuantLib::SwaptionHelper_swaptionMaturityDate(h))
      ),
      nominal = purrr::map_dbl(
        helper,
        function(h) QuantLib::SwaptionHelper_swaptionNominal(h)
      ),
      strike = purrr::map_dbl(
        helper,
        function(h) QuantLib::SwaptionHelper_swaptionStrike(h)
      ),
      optType = purrr::map_chr(
        swaption_obj,
        function(swp) QuantLib::Swaption_type(swp)
      )
    ) |>
    dplyr::select(expiry, maturity, nominal, strike, optType)
}

#' swaption calibration tbl
#'
#' Moved from `QuantLib_tidy/R/options/tidy_gaussina1d.R` (`calibration_data_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param basket,volatilities Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
swaption_calibration_tbl_GQL <- function(basket, volatilities) {
  tibble::tibble(idx = seq_len(basket$size())) |>
    dplyr::mutate(
      basket_item = purrr::map(
        idx,
        function(i) basket[i][[1]]
      ),
      modelSigma = purrr::map_dbl(
        idx,
        function(i) volatilities[i][[1]]
      ),
      helper = purrr::map(
        basket_item,
        function(x) QuantLib::as_swaption_helper(x)
      ),
      expiry = purrr::map_chr(
        helper,
        function(h) QuantLib::Date_ISO(QuantLib::SwaptionHelper_swaptionExpiryDate(h))
      ),
      modelPrice = purrr::map_dbl(
        basket_item,
        function(x) x$modelValue()
      ),
      marketPrice = purrr::map_dbl(
        basket_item,
        function(x) x$marketValue()
      ),
      modelImpVol = purrr::map2_dbl(
        basket_item,
        modelPrice,
        function(x, px) {
          QuantLib::BlackCalibrationHelper_impliedVolatility(
            self = x,
            targetValue = px,
            accuracy = 1e-6,
            maxEvaluations = 1000,
            minVol = 0.0,
            maxVol = 2.0
          )
        }
      ),
      marketImpVol = purrr::map_dbl(
        basket_item,
        function(x) QuantLib::QuoteHandle_value(x$volatility())
      )
    ) |>
    dplyr::select(expiry, modelSigma, modelPrice, marketPrice, modelImpVol, marketImpVol)
}

#' option npv at spot
#'
#' Moved from `QuantLib_tidy/R/options/tidy_graph.R` (`option_npv_at_spot()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param spot_value Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
option_npv_at_spot_GQL <- function(spot_value) {
  todaysDate <- date_GQL("1998-05-15")
  set_eval_date_GQL(todaysDate)
  
  settlementDate <- date_GQL("1998-05-17")
  
  riskFreeRate <- QuantLib::FlatForward(
    settlementDate,
    0.05,
    QuantLib::Actual365Fixed()
  )
  
  exercise <- QuantLib::EuropeanExercise(date_GQL("1999-05-17"))
  payoff <- QuantLib::PlainVanillaPayoff("Call", 8.0)
  
  underlying <- QuantLib::SimpleQuote(spot_value)
  
  volatility <- QuantLib::BlackConstantVol(
    todaysDate,
    QuantLib::TARGET(),
    0.10,
    QuantLib::Actual365Fixed()
  )
  
  dividendYield <- QuantLib::FlatForward(
    settlementDate,
    0.05,
    QuantLib::Actual365Fixed()
  )
  
  process <- QuantLib::BlackScholesMertonProcess(
    QuantLib::QuoteHandle(underlying),
    QuantLib::YieldTermStructureHandle(dividendYield),
    QuantLib::YieldTermStructureHandle(riskFreeRate),
    QuantLib::BlackVolTermStructureHandle(volatility)
  )
  
  option <- QuantLib::VanillaOption(payoff, exercise)
  QuantLib::Instrument_setPricingEngine(
    option,
    s_arg2 = QuantLib::AnalyticEuropeanEngine(process)
  )
  
  option$NPV()
}

#' option surface row
#'
#' Moved from `QuantLib_tidy/R/options/tidy_wireframe.R` (`build_option_surface_row()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param spot_value,vol_value,underlying,volatilityQuote,option Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
option_surface_row_GQL <- function(spot_value, vol_value,
                                     underlying, volatilityQuote, option) {
  underlying$setValue(value = spot_value)
  volatilityQuote$setValue(value = vol_value)
  
  tibble::tibble(
    NPV = option$NPV(),
    gamma = option$gamma(),
    delta = option$delta(),
    vega = option$vega()
  )
}

#' build corr matrix
#'
#' Moved from `QuantLib_tidy/R/options/tisy_basket.R` (`build_corr_matrix()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param rho Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
build_corr_matrix_GQL <- function(rho = 0.5) {
  m <- QuantLib::Matrix(2, 2)
  invisible(QuantLib::Matrix_setitem(m, 0, 0, 1.0))
  invisible(QuantLib::Matrix_setitem(m, 1, 1, 1.0))
  invisible(QuantLib::Matrix_setitem(m, 0, 1, rho))
  invisible(QuantLib::Matrix_setitem(m, 1, 0, rho))
  m
}

#' build process vector
#'
#' Moved from `QuantLib_tidy/R/options/tisy_basket.R` (`build_process_vector()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param process_list Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
build_process_vector_GQL <- function(process_list) {
  vec <- QuantLib::StochasticProcess1DVector(length(process_list))
  purrr::walk(
    seq_along(process_list),
    function(i) {
      invisible(
        QuantLib::StochasticProcess1DVector___setitem__(
          self = vec,
          i = i - 1,
          x = process_list[[i]]
        )
      )
    }
  )
  vec
}

#' basket european engine
#'
#' Moved from `QuantLib_tidy/R/options/tisy_basket.R` (`build_european_engine()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param process,seed Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
basket_european_engine_GQL <- function(process, seed = 42) {
  QuantLib::MCPREuropeanBasketEngine(
    process = process,
    timeSteps = NA,
    timeStepsPerYear = 1,
    brownianBridge = FALSE,
    antitheticVariate = FALSE,
    requiredSamples = NA,
    requiredTolerance = 0.02,
    maxSamples = 10000,
    seed = seed
  )
}

#' basket american engine
#'
#' Moved from `QuantLib_tidy/R/options/tisy_basket.R` (`build_american_engine()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param process,seed Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
basket_american_engine_GQL <- function(process, seed = 42) {
  QuantLib::MCPRAmericanBasketEngine(
    process = process,
    timeSteps = 10,
    timeStepsPerYear = NA,
    brownianBridge = FALSE,
    antitheticVariate = FALSE,
    requiredSamples = 50000,
    requiredTolerance = 0.02,
    maxSamples = 100000,
    seed = seed,
    nCalibrationSamples = 5000,
    polynomOrder = 5,
    polynomType = QuantLib::LsmBasisSystem_Hermite_get()
  )
}

#' price basket option
#'
#' Moved from `QuantLib_tidy/R/options/tisy_basket.R` (`price_basket_option()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param option,engine Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
price_basket_option_GQL <- function(option, engine) {
  option$setPricingEngine(engine)
  option$NPV()
}

#' append default helpers
#'
#' Moved from `QuantLib_tidy/R/swaps/tidy_CDS.R` (`append_default_helpers()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param helper_list Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
append_default_helpers_GQL <- function(helper_list) {
  vec <- QuantLib::DefaultProbabilityHelperVector()
  purrr::walk(
    helper_list,
    function(h) {
      vec$append(h)
    }
  )
  vec
}

#' extract hazard tbl
#'
#' Moved from `QuantLib_tidy/R/swaps/tidy_CDS.R` (`extract_hazard_tbl()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param hazard_curve Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
extract_hazard_tbl_GQL <- function(hazard_curve) {
  curve_dates <- hazard_curve$dates()
  hz_rates <- QuantLib::HazardRateCurve_hazardRates(hazard_curve)
  
  tibble::tibble(
    idx = seq_len(curve_dates$size())
  ) |>
    dplyr::mutate(
      date = purrr::map_chr(
        idx,
        function(i) QuantLib::Date_ISO(curve_dates[i][[1]])
      ),
      hazard_rate = purrr::map_dbl(
        idx,
        function(i) hz_rates[i]
      )
    ) |>
    dplyr::select(date, hazard_rate)
}
