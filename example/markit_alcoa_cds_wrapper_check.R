# GaussQuant wrapper check: Markit Alcoa CDS benchmark
#
# Purpose
#   1. Confirm that the public GaussQuant API covers the CDS workflow.
#   2. Run the workflow without a direct QuantLib namespace call.
#   3. Compare the available GaussQuant output with the Markit benchmark.
#
# This first-stage check uses a flat discount curve. It is therefore a wrapper
# smoke test, not yet an exact reproduction of the Markit term-structure result.

library(GaussQuant)
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Public API audit
# -----------------------------------------------------------------------------

required_gaussquant_functions <- c(
  "date_GQL",
  "set_eval_date_GQL",
  "flat_curve_GQL",
  "yield_curve_handle_GQL",
  "flat_hazard_rate_GQL",
  "make_cds_GQL",
  "cds_isda_engine_GQL",
  "cds_npv_GQL",
  "cds_fair_spread_GQL",
  "cds_summary_GQL",
  "show_tbl_GQH"
)

gaussquant_exports <- getNamespaceExports("GaussQuant")

tbl_wrapper_audit <- tibble::tibble(
  function_name = required_gaussquant_functions,
  exported = function_name %in% gaussquant_exports
)

GaussQuant::show_tbl_GQH(
  tbl_wrapper_audit,
  "GaussQuant CDS wrapper audit",
  n = 30
)

missing_gaussquant_functions <- tbl_wrapper_audit |>
  dplyr::filter(!exported) |>
  dplyr::pull(function_name)

if (length(missing_gaussquant_functions) > 0L) {
  stop(
    paste0(
      "GaussQuant wrapper functions are missing: ",
      paste(missing_gaussquant_functions, collapse = ", ")
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 2. Markit benchmark inputs
# -----------------------------------------------------------------------------

trade_date_chr <- "2014-06-24"
maturity_date_chr <- "2019-09-20"

notional <- 10000000
market_spread <- 160 / 10000
contract_coupon <- 100 / 10000
recovery_rate <- 0.40

# Markit 5Y swap rate. A flat curve is used only for this wrapper smoke test.
flat_discount_rate <- 0.017930

tbl_markit_contract <- tibble::tribble(
  ~item, ~value,
  "trade_date", trade_date_chr,
  "maturity_date", maturity_date_chr,
  "side", "buyer",
  "notional", as.character(notional),
  "market_spread_bp", as.character(10000 * market_spread),
  "contract_coupon_bp", as.character(10000 * contract_coupon),
  "recovery_rate", as.character(recovery_rate),
  "flat_discount_rate", as.character(flat_discount_rate)
)

GaussQuant::show_tbl_GQH(
  tbl_markit_contract,
  "Markit Alcoa CDS inputs",
  n = 30
)

trade_date <- GaussQuant::date_GQL(trade_date_chr)
maturity_date <- GaussQuant::date_GQL(maturity_date_chr)

GaussQuant::set_eval_date_GQL(trade_date)

# -----------------------------------------------------------------------------
# 3. Discount curve through the GaussQuant wrapper
# -----------------------------------------------------------------------------

discount_curve <- GaussQuant::flat_curve_GQL(
  reference_date = trade_date,
  rate = flat_discount_rate,
  compounding = "Continuous"
)
discount_curve_handle <- GaussQuant::yield_curve_handle_GQL(
  discount_curve
)

# -----------------------------------------------------------------------------
# 4. Calibrate a flat hazard rate to the 160 bp market spread
# -----------------------------------------------------------------------------

market_cds_npv_at_hazard <- function(hazard_rate) {
  hazard_curve_handle <- GaussQuant::flat_hazard_rate_GQL(
    hazard_rate = hazard_rate,
    reference_date = trade_date,
    day_counter = "Actual365Fixed"
  )

  pricing_engine <- GaussQuant::cds_isda_engine_GQL(
    hazard_curve_handle = hazard_curve_handle,
    recovery_rate = recovery_rate,
    discount_curve_handle = discount_curve_handle
  )

  market_cds <- GaussQuant::make_cds_GQL(
    maturity_date = maturity_date,
    running_spread = market_spread,
    notional = notional,
    side = "buyer",
    trade_date = trade_date,
    coupon_tenor = "3M",
    day_counter = "Actual360",
    date_generation_rule = "CDS2015",
    pricing_engine = pricing_engine
  )

  GaussQuant::cds_npv_GQL(market_cds)
}

hazard_calibration <- stats::uniroot(
  market_cds_npv_at_hazard,
  interval = c(1e-8, 0.50),
  tol = 1e-10
)

calibrated_hazard_rate <- hazard_calibration$root

hazard_curve_handle <- GaussQuant::flat_hazard_rate_GQL(
  hazard_rate = calibrated_hazard_rate,
  reference_date = trade_date,
  day_counter = "Actual365Fixed"
)

# -----------------------------------------------------------------------------
# 5. Price the 100 bp coupon CDS
# -----------------------------------------------------------------------------

pricing_engine <- GaussQuant::cds_isda_engine_GQL(
  hazard_curve_handle = hazard_curve_handle,
  recovery_rate = recovery_rate,
  discount_curve_handle = discount_curve_handle
)

contract_cds <- GaussQuant::make_cds_GQL(
  maturity_date = maturity_date,
  running_spread = contract_coupon,
  notional = notional,
  side = "buyer",
  trade_date = trade_date,
  coupon_tenor = "3M",
  day_counter = "Actual360",
  date_generation_rule = "CDS2015",
  pricing_engine = pricing_engine
)

gaussquant_market_value <- GaussQuant::cds_npv_GQL(
  contract_cds
)

gaussquant_fair_spread <- GaussQuant::cds_fair_spread_GQL(
  contract_cds
)

tbl_gaussquant_summary <- GaussQuant::cds_summary_GQL(
  contract_cds
)

GaussQuant::show_tbl_GQH(
  tbl_gaussquant_summary,
  "GaussQuant Alcoa CDS summary",
  n = 30
)

# -----------------------------------------------------------------------------
# 6. Compare with the Markit screenshot
# -----------------------------------------------------------------------------

tbl_markit_validation <- tibble::tribble(
  ~metric, ~markit_value, ~gaussquant_value,
  "market_value", 286052, gaussquant_market_value,
  "fair_spread_bp", 160, 10000 * gaussquant_fair_spread
) |>
  dplyr::mutate(
    difference = gaussquant_value - markit_value,
    relative_error = dplyr::if_else(
      markit_value != 0,
      difference / abs(markit_value),
      NA_real_
    )
  )

GaussQuant::show_tbl_GQH(
  tbl_markit_validation,
  "GaussQuant versus Markit: wrapper smoke test",
  n = 30
)

# -----------------------------------------------------------------------------
# 7. Interpretation
# -----------------------------------------------------------------------------

message(
  paste(
    "The script completed without a direct QuantLib namespace call.",
    "A difference from Markit's USD 286,052 is expected at this stage",
    "because the sample uses a flat 5Y discount rate instead of the full",
    "Markit deposit/swap curve."
  )
)

