# qlt_bonds_GQL.R
#
# Bond yield, asset swap, and Treasury futures basis helpers moved from QuantLib_tidy.

#' as spread from price
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base04.R` (`calc_as_spread_from_price()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param target_dirty_price,discount_price,annuity Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
as_spread_from_price_GQL <- function(target_dirty_price, discount_price, annuity) {
  (discount_price - target_dirty_price) / annuity
}

#' try make asset swap
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base04.R` (`try_make_asset_swap()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param pay_fixed_rate,bond_obj,clean_price_asw,tibor6m_for_asw,credit_spread_asw,floating_schedule_asw,is_par_asset_swap,maturity_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
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
