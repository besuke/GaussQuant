# interest_rate_curves_GQL_v4.R

library(tidyverse)
library(QuantLib)

devtools::document()
devtools::load_all()

# =============================================================================
# 1. EONIA curve bootstrapping
# =============================================================================

obj_date_today <- GaussQuant::DateParser_parseISO_GQL("2012-12-11")
GaussQuant::set_eval_date_GQL(obj_date_today)

obj_calendar_target <- QuantLib::TARGET()
obj_day_counter_actual_360 <- QuantLib::Actual360()
obj_day_counter_actual_365_fixed <- QuantLib::Actual365Fixed()
obj_period_one_day <- GaussQuant::period_GQL("1D")

tbl_deposit_quotes <- tibble::tribble(
  ~num_rate_pct, ~int_fixing_days,
  0.04, 0L,
  0.04, 1L,
  0.04, 2L
)

lst_helper_deposit <- tbl_deposit_quotes |>
  dplyr::mutate(
    num_rate = num_rate_pct / 100,
    obj_quote_handle = purrr::map(
      num_rate,
      \(num_rate) QuantLib::QuoteHandle(QuantLib::SimpleQuote(num_rate))
    ),
    obj_rate_helper = purrr::map2(
      obj_quote_handle,
      int_fixing_days,
      \(obj_quote_handle, int_fixing_days) {
        QuantLib::DepositRateHelper(
          obj_quote_handle,
          obj_period_one_day,
          int_fixing_days,
          obj_calendar_target,
          "Following",
          FALSE,
          obj_day_counter_actual_360
        )
      }
    )
  ) |>
  dplyr::pull(obj_rate_helper)

obj_index_eonia <- QuantLib::Eonia()

tbl_ois_short_quotes <- tibble::tribble(
  ~num_rate_pct, ~int_tenor, ~str_unit,
  0.070, 1L, "Weeks",
  0.069, 2L, "Weeks",
  0.078, 3L, "Weeks",
  0.074, 1L, "Months"
)

lst_helper_ois_short <- tbl_ois_short_quotes |>
  dplyr::mutate(
    num_rate = num_rate_pct / 100,
    obj_period = purrr::map2(
      int_tenor,
      str_unit,
      \(int_tenor, str_unit) GaussQuant::period_GQL(int_tenor, str_unit)
    ),
    obj_quote_handle = purrr::map(
      num_rate,
      \(num_rate) QuantLib::QuoteHandle(QuantLib::SimpleQuote(num_rate))
    ),
    obj_rate_helper = purrr::map2(
      obj_period,
      obj_quote_handle,
      \(obj_period, obj_quote_handle) {
        QuantLib::OISRateHelper(
          2L,
          obj_period,
          obj_quote_handle,
          obj_index_eonia
        )
      }
    )
  ) |>
  dplyr::pull(obj_rate_helper)

tbl_ois_dated_quotes <- tibble::tribble(
  ~num_rate_pct, ~str_start_date, ~str_end_date,
   0.046, "2013-01-16", "2013-02-13",
   0.016, "2013-02-13", "2013-03-13",
  -0.007, "2013-03-13", "2013-04-10",
  -0.013, "2013-04-10", "2013-05-08",
  -0.014, "2013-05-08", "2013-06-12"
)
lst_helper_ois_dated <- tbl_ois_dated_quotes |>
  dplyr::mutate(
    num_rate = num_rate_pct / 100,

    obj_quote_handle = purrr::map(
      num_rate,
      \(num_rate) {
        QuantLib::QuoteHandle(
          QuantLib::SimpleQuote(num_rate)
        )
      }
    ),

    obj_rate_helper = purrr::pmap(
      list(
        str_start_date,
        str_end_date,
        obj_quote_handle
      ),
      \(str_start_date, str_end_date, obj_quote_handle) {
        GaussQuant::DatedOISRateHelper_GQL(
          start_date = str_start_date,
          end_date = str_end_date,
          quote_handle = obj_quote_handle,
          overnight_index = obj_index_eonia
        )
      }
    )
  ) |>
  dplyr::pull(obj_rate_helper)

tbl_ois_long_quotes <- tibble::tribble(
  ~num_rate_pct, ~int_tenor, ~str_unit,
  0.002, 15L, "Months",
  0.008, 18L, "Months",
  0.021, 21L, "Months",
  0.036, 2L, "Years",
  0.127, 3L, "Years",
  0.274, 4L, "Years",
  0.456, 5L, "Years",
  0.647, 6L, "Years",
  0.827, 7L, "Years",
  0.996, 8L, "Years",
  1.147, 9L, "Years",
  1.280, 10L, "Years",
  1.404, 11L, "Years",
  1.516, 12L, "Years",
  1.764, 15L, "Years",
  1.939, 20L, "Years",
  2.003, 25L, "Years",
  2.038, 30L, "Years"
)

lst_helper_ois_long <- tbl_ois_long_quotes |>
  dplyr::mutate(
    num_rate = num_rate_pct / 100,
    obj_period = purrr::map2(
      int_tenor,
      str_unit,
      \(int_tenor, str_unit) GaussQuant::period_GQL(int_tenor, str_unit)
    ),
    obj_quote_handle = purrr::map(
      num_rate,
      \(num_rate) QuantLib::QuoteHandle(QuantLib::SimpleQuote(num_rate))
    ),
    obj_rate_helper = purrr::map2(
      obj_period,
      obj_quote_handle,
      \(obj_period, obj_quote_handle) {
        QuantLib::OISRateHelper(
          2L,
          obj_period,
          obj_quote_handle,
          obj_index_eonia
        )
      }
    )
  ) |>
  dplyr::pull(obj_rate_helper)

lst_rate_helpers <- c(
  lst_helper_deposit,
  lst_helper_ois_short,
  lst_helper_ois_dated,
  lst_helper_ois_long
)

obj_rate_helper_vector <- GaussQuant::push_rate_helpers_GQL(lst_rate_helpers)

obj_curve_eonia_log_cubic_raw <- QuantLib::PiecewiseLogCubicDiscount(
  0L,
  obj_calendar_target,
  obj_rate_helper_vector,
  obj_day_counter_actual_365_fixed
)

obj_curve_eonia_log_cubic_raw$enableExtrapolation()

obj_date_reference <- obj_curve_eonia_log_cubic_raw$referenceDate()
obj_date_end_two_years <- obj_date_reference + GaussQuant::period_GQL("2Y")

vec_serial_daily <- seq.int(
  obj_date_reference$serialNumber(),
  obj_date_end_two_years$serialNumber()
)

lst_date_daily <- purrr::map(vec_serial_daily, QuantLib::Date)

vec_forward_rate_log_cubic_raw <- purrr::map_dbl(
  lst_date_daily,
  \(obj_date) {
    obj_date_next <- obj_calendar_target$advance(obj_date, 1L, "Days")
    obj_curve_eonia_log_cubic_raw$forwardRate(
      obj_date,
      obj_date_next,
      obj_day_counter_actual_360,
      "Simple"
    )$rate()
  }
)

tbl_forward_log_cubic_raw <- tibble::tibble(
  date = as.Date(
    purrr::map_chr(
      lst_date_daily,
      GaussQuant::str_date_ISO_GQL
    )
  ),
  num_forward_rate = vec_forward_rate_log_cubic_raw
)

tbl_forward_log_cubic_raw

ggplot2::ggplot(
  tbl_forward_log_cubic_raw,
  ggplot2::aes(
    x = date,
    y = num_forward_rate
  )
) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.001)
  ) +
  ggplot2::labs(
    title = "EONIA daily overnight forwards: raw log-cubic curve",
    x = NULL,
    y = "Forward rate"
  ) +
  ggplot2::theme_minimal()

obj_curve_eonia_flat_forward_raw <- QuantLib::PiecewiseFlatForward(
  0L,
  obj_calendar_target,
  obj_rate_helper_vector,
  obj_day_counter_actual_365_fixed
)

obj_curve_eonia_flat_forward_raw$enableExtrapolation()

obj_date_end_six_months <- obj_date_reference + GaussQuant::period_GQL("6M")

vec_serial_six_months <- seq.int(
  obj_date_reference$serialNumber(),
  obj_date_end_six_months$serialNumber()
)

lst_date_six_months <- purrr::map(vec_serial_six_months, QuantLib::Date)

vec_forward_rate_flat_forward_raw <- purrr::map_dbl(
  lst_date_six_months,
  \(obj_date) {
    obj_date_next <- obj_calendar_target$advance(obj_date, 1L, "Days")
    obj_curve_eonia_flat_forward_raw$forwardRate(
      obj_date,
      obj_date_next,
      obj_day_counter_actual_360,
      "Simple"
    )$rate()
  }
)

tbl_forward_flat_forward_raw <- tibble::tibble(
  date = as.Date(
    purrr::map_chr(
      lst_date_six_months,
      GaussQuant::str_date_ISO_GQL
    )
  ),
  num_forward_rate = vec_forward_rate_flat_forward_raw
)

tbl_forward_flat_forward_raw

obj_nodes_raw <- obj_curve_eonia_flat_forward_raw$nodes()
obj_nodes_raw

# The remaining Cookbook steps depend on how this QuantLib R binding exposes
# the node vector. After confirming node extraction, continue with:
# 1. replace node 7 by the average of nodes 6 and 8;
# 2. construct ForwardCurve from the modified nodes;
# 3. estimate the turn-of-year jump;
# 4. build PiecewiseFlatForward and PiecewiseLogCubicDiscount with jump vectors.

tbl_wrapper_audit_eonia <- tibble::tribble(
  ~str_area, ~str_current_state, ~str_candidate,
  "ISO date parsing", "covered", "DateParser_parseISO_GQL",
  "ISO date formatting", "covered", "str_date_ISO_GQL",
  "Period construction", "covered", "period_GQL",
  "RateHelperVector construction", "covered", "push_rate_helpers_GQL",
  "DepositRateHelper construction", "raw QuantLib", "deposit_rate_helper_GQL",
  "OISRateHelper construction", "raw QuantLib", "ois_rate_helper_GQL",
  "DatedOISRateHelper construction", "raw QuantLib", "dated_ois_rate_helper_GQL",
  "Eonia index construction", "raw QuantLib", "eonia_GQL",
  "Piecewise curve construction", "raw QuantLib", "piecewise_curve_GQL",
  "Daily forward-rate extraction", "analysis code", "tbl_forward_curve_GQL",
  "Curve-node extraction", "binding-dependent", "tbl_curve_nodes_GQL"
)

tbl_wrapper_audit_eonia

# =============================================================================
# 2. Euribor curve bootstrapping
# =============================================================================

# Add after the EONIA section runs and node extraction is confirmed.
