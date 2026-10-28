# qlt_bonds_GQL.R
#
# Bond yield, asset swap, and Treasury futures basis helpers moved from QuantLib_tidy.

#' bond clean price arg
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base04.R` (`bond_clean_price_arg()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param x Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
bond_clean_price_arg_GQL <- function(x) {
  out <- tryCatch(QuantLib::BondPrice(x, "Clean"), error = function(e) NULL)
  if (!is.null(out)) return(out)
  x
}

#' bond yield safe
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base04.R` (`bond_yield_safe()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param bond_obj,clean_price,day_count_obj,compounding,frequency Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
bond_yield_safe_GQL <- function(bond_obj, clean_price, day_count_obj, compounding, frequency) {
  cp <- bond_clean_price_arg_GQL(clean_price)
  
  out <- tryCatch(
    bond_obj$bondYield(cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  out <- tryCatch(
    BondFunctions_bondYield(bond_obj, cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  out <- tryCatch(
    bond_obj$yield(cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  stop("Could not compute bond yield in this SWIG build")
}

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

#' make us tsy bond
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base05.R` (`make_us_tsy_bond()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param effective_date,maturity_date,coupon_rate_pct,face_amount,settlement_days Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
make_us_tsy_bond_GQL <- function(effective_date,
                             maturity_date,
                             coupon_rate_pct,
                             face_amount = 100.0,
                             settlement_days = 1L) {
  effective_date_ql <- date_GQL(effective_date)
  maturity_date_ql <- date_GQL(maturity_date)
  
  bond_schedule <- QuantLib::Schedule(
    effective_date_ql,
    maturity_date_ql,
    period_months_GQL(6),
    QuantLib::UnitedStates("GovernmentBond"),
    "Unadjusted",
    "Unadjusted",
    "Backward",
    FALSE
  )
  
  bond_obj <- QuantLib::FixedRateBond(
    settlement_days,
    face_amount,
    bond_schedule,
    c(coupon_rate_pct / 100),
    QuantLib::ActualActual("Bond")
  )
  
  list(
    bond = bond_obj,
    schedule = bond_schedule,
    effective_date = effective_date_ql,
    maturity_date = maturity_date_ql,
    coupon_rate_pct = coupon_rate_pct,
    face_amount = face_amount
  )
}

#' calc gross basis row
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base05.R` (`calc_gross_basis_row()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param issue_date,maturity_date,coupon_rate_pct,conversion_factor,market_yield_pct,futures_price,settle_date Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
calc_gross_basis_row_GQL <- function(issue_date,
                                 maturity_date,
                                 coupon_rate_pct,
                                 conversion_factor,
                                 market_yield_pct, futures_price, settle_date) {
  bond_i <- make_us_tsy_bond_GQL(issue_date, maturity_date, coupon_rate_pct)$bond
  ir_i <- QuantLib::InterestRate(
    market_yield_pct / 100,
    QuantLib::ActualActual("Bond"),
    QuantLib::Compounding_Compounded_get(),
    QuantLib::Frequency_Semiannual_get()
  )
  
  bpv_value <- QuantLib::BondFunctions_basisPointValue(bond_i, ir_i)
  clean_price_i <- bond_i$cleanPrice(
    market_yield_pct / 100,
    QuantLib::ActualActual("Bond"),
    QuantLib::Compounding_Compounded_get(),
    QuantLib::Frequency_Semiannual_get(),
    settle_date
  )
  dirty_price_i <- bond_i$dirtyPrice(
    market_yield_pct / 100,
    QuantLib::ActualActual("Bond"),
    QuantLib::Compounding_Compounded_get(),
    QuantLib::Frequency_Semiannual_get(),
    settle_date
  )
  gross_basis <- clean_price_i - futures_price * conversion_factor
  
  tibble::tibble(
    issue_date = issue_date,
    maturity = iso_GQL(bond_i$maturityDate()),
    coupon = bond_i$nextCouponRate(),
    yield_pct = market_yield_pct,
    bpv = bpv_value,
    clean_price = clean_price_i,
    dirty_price = dirty_price_i,
    conversion_factor = conversion_factor,
    gross_basis = gross_basis,
    bond_obj = list(bond_i)
  )
}

#' calc net basis row
#'
#' Moved from `QuantLib_tidy/R/bonds_basic/base05.R` (`calc_net_basis_row()`) without changing
#' the calculation. Script-level constants and data are passed explicitly.
#'
#' @param bond_obj,conversion_factor,clean_price,dirty_price,gross_basis,futures_price,settle_date,repo_end_date,repo_rate,repo_year_frac Arguments of the original helper.
#' @return The value returned by the original QuantLib_tidy helper.
#' @export
calc_net_basis_row_GQL <- function(bond_obj,
                               conversion_factor,
                               clean_price,
                               dirty_price,
                               gross_basis, futures_price, settle_date, repo_end_date, repo_rate, repo_year_frac) {
  accrued_start <- bond_obj$accruedAmount(settle_date)
  accrued_end <- bond_obj$accruedAmount(repo_end_date)
  coupon_income <- accrued_end - accrued_start
  repo_cost <- repo_rate * repo_year_frac * dirty_price
  carry <- coupon_income - repo_cost
  net_basis <- gross_basis - carry
  forward_price <- clean_price - carry
  implied_repo <- ((futures_price * conversion_factor + accrued_end) / dirty_price - 1) / repo_year_frac
  
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

