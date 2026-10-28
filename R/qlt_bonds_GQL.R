# qlt_bonds_GQL.R
#
# Bond yield, asset swap, and Treasury futures basis helpers moved from QuantLib_tidy.

#' Asset swap spread implied by a bond price
#'
#' Solves the par asset swap identity for the spread: the gap between the
#' price of the bond's cashflows discounted on the swap curve and the bond's
#' dirty price, divided by the annuity (PV of one unit of spread per year on
#' the floating leg).
#'
#' @param target_dirty_price Dirty price of the bond, per 100 notional.
#' @param discount_price Price of the bond cashflows discounted on the swap
#'   curve, per 100 notional.
#' @param annuity Annuity of the floating leg, per 100 notional.
#'
#' @return Numeric spread, in the same units as `annuity` implies (decimal
#'   when the annuity is per unit of rate).
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base04.R`.
#' @export
as_spread_from_price_GQL <- function(target_dirty_price, discount_price, annuity) {
  (discount_price - target_dirty_price) / annuity
}

#' Build a TIBOR asset swap on a fixed-rate bond
#'
#' Calls the `AssetSwap` constructor with gearing 1, non-par repayment 100 and
#' an Actual/365 (Fixed) floating day counter, as in the QuantLib_tidy bond
#' chapter. The constructor signature differs between QuantLib-SWIG builds, so
#' a failure returns `NULL` instead of raising an error.
#'
#' @param pay_fixed_rate `TRUE` to pay the bond's fixed coupons and receive
#'   the floating leg.
#' @param bond_obj QuantLib bond.
#' @param clean_price_asw Bond clean price used for the asset swap.
#' @param tibor6m_for_asw Floating-rate index, for example 6M TIBOR.
#' @param credit_spread_asw Spread over the floating index, decimal.
#' @param floating_schedule_asw QuantLib schedule of the floating leg.
#' @param is_par_asset_swap `TRUE` for a par asset swap, `FALSE` for a
#'   market-value asset swap.
#' @param maturity_date QuantLib Date of the floating leg maturity.
#'
#' @return A QuantLib `AssetSwap`, or `NULL` if the constructor fails.
#' @note Moved from QuantLib_tidy: `R/bonds_basic/base04.R`.
#' @export
try_make_asset_swap_GQL <- function(pay_fixed_rate, bond_obj, clean_price_asw, tibor6m_for_asw, credit_spread_asw, floating_schedule_asw, is_par_asset_swap, maturity_date) {
  tryCatch(
    QuantLib::AssetSwap__SWIG_0(
      pay_fixed_rate,
      bond_obj,
      clean_price_asw,
      tibor6m_for_asw,
      credit_spread_asw,
      floating_schedule_asw,
      QuantLib::Actual365Fixed(),
      is_par_asset_swap,
      1.0,
      100.0,
      maturity_date
    ),
    error = function(e) NULL
  )
}
