# example/tonar_interest_rate_curve_GQL.R

library(tidyverse)
library(QuantLib)

devtools::document()
devtools::load_all()

# =============================================================================
# 1. TONAR market quotes
# =============================================================================

str_evaluation_date <- "2026-01-20"

obj_evaluation_date <-
  GaussQuant::DateParser_parseISO_GQL(
    str_evaluation_date
  )

GaussQuant::set_eval_date_GQL(
  obj_evaluation_date
)

tbl_tonar_quotes <-
  GaussQuant::jscc_tonar_quotes_GQL()

tbl_tonar_quotes

tbl_tonar_quotes |>
  dplyr::filter(
    quote_id == "TONAR-5Y"
  )

# =============================================================================
# 2. Build log-cubic-discount and flat-forward curves
# =============================================================================

lst_tonar_log_cubic <-
  GaussQuant::tonar_curve_benchmark_GQL(
    quotes = tbl_tonar_quotes,
    evaluation_date = str_evaluation_date,
    curve_type = "log_cubic_discount"
  )

lst_tonar_flat_forward <-
  GaussQuant::tonar_curve_benchmark_GQL(
    quotes = tbl_tonar_quotes,
    evaluation_date = str_evaluation_date,
    curve_type = "flat_forward"
  )

# =============================================================================
# 3. Validate market-quote repricing
# =============================================================================

tbl_tonar_repricing_log_cubic <-
  lst_tonar_log_cubic$
    validation$
    quote_repricing

tbl_tonar_repricing_flat_forward <-
  lst_tonar_flat_forward$
    validation$
    quote_repricing

tbl_tonar_repricing_log_cubic |>
  dplyr::select(
    quote_id,
    market_quote,
    implied_quote,
    quote_error,
    pillar_date,
    discount_factor,
    zero_rate,
    inst_fwd_rate
  )

tbl_tonar_repricing_flat_forward |>
  dplyr::select(
    quote_id,
    market_quote,
    implied_quote,
    quote_error,
    pillar_date,
    discount_factor,
    zero_rate,
    inst_fwd_rate
  )

tbl_tonar_validation_summary <-
  tibble::tribble(
    ~curve_type, ~max_abs_quote_error,
    "log_cubic_discount",
    max(
      abs(
        tbl_tonar_repricing_log_cubic$
          quote_error
      ),
      na.rm = TRUE
    ),
    "flat_forward",
    max(
      abs(
        tbl_tonar_repricing_flat_forward$
          quote_error
      ),
      na.rm = TRUE
    )
  )

tbl_tonar_validation_summary

# =============================================================================
# 4. Extract curve nodes safely
# =============================================================================

tbl_tonar_nodes_log_cubic <-
  GaussQuant::tonar_curve_nodes_GQL(
    lst_tonar_log_cubic$
      curve_bundle$
      curve
  )

tbl_tonar_nodes_flat_forward <-
  GaussQuant::tonar_curve_nodes_GQL(
    lst_tonar_flat_forward$
      curve_bundle$
      curve
  )

tbl_tonar_nodes_log_cubic
tbl_tonar_nodes_flat_forward

# Do not call curve$nodes() directly. Some QuantLib/SWIG environments can
# overflow the R node stack while converting the returned C++ node vector.

# =============================================================================
# 5. Calculate daily instantaneous forward rates
# =============================================================================

vec_curve_dates <-
  seq.Date(
    from = as.Date(str_evaluation_date),
    to = as.Date("2031-01-20"),
    by = "day"
  )

vec_date_iso <-
  as.character(
    vec_curve_dates
  )

vec_inst_fwd_rate_log_cubic <-
  purrr::map_dbl(
    vec_date_iso,
    \(str_date) {
      GaussQuant::tonar_forward_rate_GQL(
        curve =
          lst_tonar_log_cubic$
            curve_bundle$
            curve,
        start_date = str_date,
        end_date = str_date
      )
    }
  )

vec_inst_fwd_rate_flat_forward <-
  purrr::map_dbl(
    vec_date_iso,
    \(str_date) {
      GaussQuant::tonar_forward_rate_GQL(
        curve =
          lst_tonar_flat_forward$
            curve_bundle$
            curve,
        start_date = str_date,
        end_date = str_date
      )
    }
  )

tbl_tonar_inst_fwd_curve <-
  tibble::tibble(
    date = vec_curve_dates,
    log_cubic_discount =
      vec_inst_fwd_rate_log_cubic,
    flat_forward =
      vec_inst_fwd_rate_flat_forward
  ) |>
  tidyr::pivot_longer(
    cols = c(
      log_cubic_discount,
      flat_forward
    ),
    names_to = "curve_type",
    values_to = "inst_fwd_rate"
  )

tbl_tonar_inst_fwd_curve

ggplot2::ggplot(
  tbl_tonar_inst_fwd_curve,
  ggplot2::aes(
    x = date,
    y = inst_fwd_rate,
    colour = curve_type
  )
) +
  ggplot2::geom_line(
    linewidth = 0.7
  ) +
  ggplot2::scale_y_continuous(
    labels =
      scales::label_percent(
        accuracy = 0.01
      )
  ) +
  ggplot2::labs(
    title = "TONAR instantaneous forward curves",
    subtitle = "JPY OIS market data as of 2026-01-20",
    x = NULL,
    y = "Instantaneous forward rate",
    colour = "Curve type"
  ) +
  ggplot2::theme_minimal()

# =============================================================================
# 6. Compare five-year curve values
# =============================================================================

str_five_year_date <- "2031-01-20"

tbl_tonar_five_year_comparison <-
  tibble::tribble(
    ~curve_type, ~inst_fwd_rate,
    "log_cubic_discount",
    GaussQuant::tonar_forward_rate_GQL(
      curve =
        lst_tonar_log_cubic$
          curve_bundle$
          curve,
      start_date = str_five_year_date,
      end_date = str_five_year_date
    ),
    "flat_forward",
    GaussQuant::tonar_forward_rate_GQL(
      curve =
        lst_tonar_flat_forward$
          curve_bundle$
          curve,
      start_date = str_five_year_date,
      end_date = str_five_year_date
    )
  )

tbl_tonar_five_year_comparison
