# qlt_models_GQL.R
#
# Short-rate, Gaussian1d, option, basket and credit helpers moved from QuantLib_tidy.

#' Zero-coupon bond price from a short-rate model
#'
#' Calls the model's `discountBond(t0, t1, r)`, falling back to
#' `discountBond(t1, r)` for models that expose only that form.
#'
#' @param model_obj QuantLib short-rate model such as Vasicek, CIR or
#'   Hull-White; `NULL` gives `NA`.
#' @param t0,t1 Start and maturity times in years.
#' @param short_rate Short rate at `t0`, decimal.
#'
#' @return Numeric discount bond price, or `NA` if neither call works.
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base06.R`.
#' @export
safe_discount_bond_GQL <- function(model_obj, t0, t1, short_rate = 0.03) {
  if (is.null(model_obj)) return(NA_real_)
  
  out <- tryCatch(model_obj$discountBond(t0, t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(model_obj$discountBond(t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  NA_real_
}

#' Simulate one Vasicek short-rate path
#'
#' Euler discretisation of
#' \eqn{dr = \kappa(\theta - r)\,dt + \sigma\,dW}.
#' Calls `set.seed(seed)`, which resets the global random number state.
#'
#' @param n_steps Number of time steps.
#' @param dt Step length in years.
#' @param r0 Initial short rate, decimal.
#' @param kappa Mean-reversion speed.
#' @param theta Long-run mean, decimal.
#' @param sigma Volatility.
#' @param seed Random seed.
#'
#' @return A tibble with `step`, `time` and `short_rate` (`n_steps + 1` rows).
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base06.R`.
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

#' Simulate one CIR short-rate path
#'
#' Euler discretisation of
#' \eqn{dr = \kappa(\theta - r)\,dt + \sigma\sqrt{r}\,dW},
#' with the rate floored at zero inside the square root and after each step.
#' Calls `set.seed(seed)`, which resets the global random number state.
#'
#' @inheritParams simulate_vasicek_path_GQL
#'
#' @return A tibble with `step`, `time` and `short_rate` (`n_steps + 1` rows).
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base06.R`.
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

#' Build a European payer swaption on 6M Euribor
#'
#' The underlying swap pays an annual 30/360 (Bond Basis) fixed leg and
#' receives semiannual 6M Euribor (Actual/360), on TARGET schedules with
#' Modified Following and backward generation. No pricing engine is set, so a
#' Hull-White (Jamshidian) or other swaption engine can be attached.
#'
#' @param curve_handle Forecasting curve handle for Euribor.
#' @param exercise_date Exercise date.
#' @param swap_start,swap_maturity Underlying swap start and end dates.
#' @param nominal Notional.
#' @param fixed_rate Fixed rate of the underlying swap, decimal.
#'
#' @return A list with `fixed_schedule`, `float_schedule`, `index`, `swap`,
#'   `exercise` and `swaption`.
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base06.R`.
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

#' Tabulate survival probabilities of a default curve
#'
#' Evaluates survival probabilities on an even time grid up to the curve's
#' maximum time and derives an average hazard rate \eqn{-\log(S(t))/t}.
#' When the curve does not report a maximum time or reference date, 10 years
#' and 2023-01-03 are used.
#'
#' @param default_curve_obj QuantLib default probability term structure.
#' @param n Number of grid points.
#'
#' @return A tibble with `time`, `survival_probability`, `hazard_rate_proxy`
#'   and `curve_date` (reference date plus `round(time * 365)` days).
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base08.R`.
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

#' Price a European option under one spot and volatility scenario
#'
#' Builds a Black-Scholes-Merton process with a flat volatility and prices the
#' option analytically. The dividend and risk-free curves are kept fixed.
#'
#' @param spot Underlying spot.
#' @param volatility Flat Black volatility, decimal.
#' @param eval_date QuantLib evaluation Date for the volatility curve.
#' @param calendar_obj Calendar of the volatility curve.
#' @param day_counter_obj Day counter of the volatility curve.
#' @param dividend_handle Dividend yield curve handle.
#' @param risk_free_handle Risk-free curve handle.
#' @param payoff_call QuantLib payoff, for example a plain vanilla call.
#' @param european_exercise QuantLib European exercise.
#'
#' @return Numeric NPV, or `NA` if the engine cannot be built or set.
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base09.R`.
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

#' Price an FX option under one spot, volatility and domestic rate scenario
#'
#' Garman-Kohlhagen pricing through a Black-Scholes-Merton process: the
#' foreign rate plays the role of the dividend yield and the domestic rate the
#' risk-free rate, both as flat curves.
#'
#' @param spot FX spot, domestic per foreign unit.
#' @param volatility Flat Black volatility, decimal.
#' @param domestic_rate_local Domestic interest rate, decimal.
#' @param eval_date QuantLib evaluation Date for the curves.
#' @param calendar_obj Calendar of the volatility curve.
#' @param day_counter_obj Day counter of the curves.
#' @param foreign_rate Foreign interest rate, decimal.
#' @param payoff_call QuantLib payoff.
#' @param exercise_obj QuantLib exercise.
#'
#' @return Numeric NPV, or `NA` if the engine cannot be built or set.
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base10.R`.
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

#' Collect quote handles into a QuantLib QuoteHandleVector
#'
#' @param qh_list List of QuantLib `QuoteHandle` objects.
#'
#' @return A QuantLib `QuoteHandleVector`.
#' @note Moved from QuantLib_tidy: `R/options/tidy_gaussina1d.R`.
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

#' Bermudan exercise dates from a swap schedule
#'
#' Moves every schedule date back by `lag_days` business days and drops the
#' first and last dates, giving exercise dates that precede each coupon start.
#'
#' @param schedule_obj QuantLib schedule of the underlying swap.
#' @param calendar Calendar used for the shift.
#' @param lag_days Business days between exercise and coupon start.
#'
#' @return A QuantLib `DateVector`.
#' @note Moved from QuantLib_tidy: `R/options/tidy_gaussina1d.R`.
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

#' Attach a pricing engine to every swaption helper in a basket
#'
#' Modifies the helpers in place.
#'
#' @param basket QuantLib calibration helper vector of swaption helpers.
#' @param engine Swaption pricing engine, for example a Gaussian1d engine.
#'
#' @return `NULL`, invisibly.
#' @note Moved from QuantLib_tidy: `R/options/tidy_gaussina1d.R`.
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

#' Tabulate the swaptions in a calibration basket
#'
#' @param basket QuantLib calibration helper vector of swaption helpers.
#'
#' @return A tibble with `expiry`, `maturity`, `nominal`, `strike` and
#'   `optType`.
#' @note Moved from QuantLib_tidy: `R/options/tidy_gaussina1d.R`.
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

#' Compare model and market values of a calibrated swaption basket
#'
#' Model implied volatilities invert each helper's model price with
#' `impliedVolatility()` (accuracy 1e-6, at most 1000 evaluations, volatility
#' between 0 and 2).
#'
#' @param basket QuantLib calibration helper vector of swaption helpers.
#' @param volatilities Calibrated model volatilities, one per helper.
#'
#' @return A tibble with `expiry`, `modelSigma`, `modelPrice`, `marketPrice`,
#'   `modelImpVol` and `marketImpVol`.
#' @note Moved from QuantLib_tidy: `R/options/tidy_gaussina1d.R`.
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

#' NPV of the QuantLib example European call for a given spot
#'
#' Uses the fixed setup of the QuantLib European option example: evaluation
#' date 1998-05-15, settlement 1998-05-17, expiry 1999-05-17, strike 8, flat
#' risk-free rate and dividend yield of 5%, and volatility 10%. Sets the
#' global QuantLib evaluation date to 1998-05-15.
#'
#' @param spot_value Underlying spot.
#'
#' @return Numeric NPV.
#' @note Moved from QuantLib_tidy: `R/options/tidy_graph.R`.
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

#' NPV and Greeks of an option for one spot and volatility
#'
#' Sets the given values on the spot and volatility quotes, which updates the
#' option through its observers, and reads the NPV and Greeks. The quotes are
#' left at the new values.
#'
#' @param spot_value,vol_value Spot and volatility to set.
#' @param underlying QuantLib `SimpleQuote` of the spot.
#' @param volatilityQuote QuantLib `SimpleQuote` of the volatility.
#' @param option QuantLib option with a pricing engine linked to the quotes.
#'
#' @return A one-row tibble with `NPV`, `gamma`, `delta` and `vega`.
#' @note Moved from QuantLib_tidy: `R/options/tidy_wireframe.R`.
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

#' Two-asset correlation matrix
#'
#' @param rho Correlation between the two assets.
#'
#' @return A 2 x 2 QuantLib `Matrix`.
#' @note Moved from QuantLib_tidy: `R/options/tisy_basket.R`.
#' @export
build_corr_matrix_GQL <- function(rho = 0.5) {
  m <- QuantLib::Matrix(2, 2)
  invisible(QuantLib::Matrix_setitem(m, 0, 0, 1.0))
  invisible(QuantLib::Matrix_setitem(m, 1, 1, 1.0))
  invisible(QuantLib::Matrix_setitem(m, 0, 1, rho))
  invisible(QuantLib::Matrix_setitem(m, 1, 0, rho))
  m
}

#' Collect one-dimensional processes into a QuantLib vector
#'
#' @param process_list List of QuantLib one-dimensional stochastic processes.
#'
#' @return A QuantLib `StochasticProcess1DVector`, for example for a
#'   `StochasticProcessArray`.
#' @note Moved from QuantLib_tidy: `R/options/tisy_basket.R`.
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

#' Monte Carlo engine for European basket options
#'
#' Pseudo-random `MCPREuropeanBasketEngine` with one time step per year,
#' required tolerance 0.02 and at most 10000 samples.
#'
#' @param process Multi-dimensional process, for example a
#'   `StochasticProcessArray`.
#' @param seed Random seed of the engine.
#'
#' @return A QuantLib pricing engine.
#' @note Moved from QuantLib_tidy: `R/options/tisy_basket.R`.
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

#' Least-squares Monte Carlo engine for American basket options
#'
#' Pseudo-random `MCPRAmericanBasketEngine` with 10 time steps, 50000
#' required samples (tolerance 0.02, at most 100000), 5000 calibration
#' samples and a fifth-order Hermite basis.
#'
#' @inheritParams basket_european_engine_GQL
#'
#' @return A QuantLib pricing engine.
#' @note Moved from QuantLib_tidy: `R/options/tisy_basket.R`.
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

#' Price a basket option with a given engine
#'
#' Sets `engine` on `option` (modifying it) and returns the NPV.
#'
#' @param option QuantLib basket option.
#' @param engine Pricing engine.
#'
#' @return Numeric NPV.
#' @note Moved from QuantLib_tidy: `R/options/tisy_basket.R`.
#' @export
price_basket_option_GQL <- function(option, engine) {
  option$setPricingEngine(engine)
  option$NPV()
}

#' Collect default probability helpers into a QuantLib vector
#'
#' @param helper_list List of QuantLib default probability helpers, for
#'   example CDS spread helpers.
#'
#' @return A QuantLib `DefaultProbabilityHelperVector`.
#' @note Moved from QuantLib_tidy: `R/swaps/tidy_CDS.R`.
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

#' Tabulate the nodes of a hazard rate curve
#'
#' @param hazard_curve QuantLib hazard rate curve, for example a bootstrapped
#'   piecewise flat hazard curve.
#'
#' @return A tibble with `date` (ISO) and `hazard_rate`.
#' @note Moved from QuantLib_tidy: `R/swaps/tidy_CDS.R`.
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
