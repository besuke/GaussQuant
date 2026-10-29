# OIS-based par-par asset swaps (migrated from LagrangeFinance R/).
# Unlike make_asset_swap_GQL(), which wraps QuantLib's AssetSwap with an IBOR
# floating leg, these functions value a daily-compounded overnight (OIS) leg
# built with MakeOIS and discount every cash flow on the OIS curve.

.ois_asw_table_GQL <- function(leg) {
  out <- cashflow_standardise_GQL(leg_to_cashflow_tbl_GQL(leg))
  if (anyNA(out$payment_date) || any(!is.finite(out$amount))) {
    stop("Invalid cashflow dates or amounts.", call. = FALSE)
  }
  out
}

.ois_asw_discount_GQL <- function(curve, dates) {
  purrr::map_dbl(dates, function(d) {
    value <- as.numeric(curve$discount(date_GQL(as.Date(d, origin = "1970-01-01"))))
    if (length(value) != 1L || !is.finite(value) || value <= 0) {
      stop("Invalid discount factor.", call. = FALSE)
    }
    value
  })
}

#' OIS-based par-par asset swap
#'
#' Values a par-par asset swap of a fixed-rate bullet bond against a
#' daily-compounded overnight index leg. The investor pays the bond coupons
#' and receives the overnight leg plus a spread; the difference between the
#' dirty price and par is exchanged at the start.
#'
#' This is distinct from [make_asset_swap_GQL()], which wraps QuantLib's
#' `AssetSwap` with an IBOR floating leg (simple term-rate coupons). Here the
#' floating leg is an OIS leg from `MakeOIS`, forecast and discounted on the
#' OIS curve returned by [ir_build_ois_curve_GQH()].
#'
#' @param bond QuantLib fixed-rate bullet bond, e.g. from [fixed_rate_bond_GQL()].
#' @param clean_price Clean price per 100.
#' @param ois_curve OIS curve environment from [ir_build_ois_curve_GQH()].
#' @param spread_bp Contract spread on the overnight leg in basis points.
#' @param payment_frequency Overnight leg payment frequency.
#' @param payment_lag Overnight leg payment lag in business days.
#' @param index Overnight index that forecasts the floating leg.
#' @param discount_curve Curve used to discount all cash flows.
#' @param spread_day_counter Day counter of the spread annuity; defaults to the
#'   index day counter.
#'
#' @return An object of class `ois_asset_swap_GQL`: a list with `valuation`
#'   (a one-row tibble with `fair_spread_bp`, `npv`, `spread_annuity` and the
#'   leg present values), signed `cashflows`, and the effective and maturity
#'   dates.
#' @seealso [ois_asset_swap_fair_price_GQL()], [ois_asset_swap_price_table_GQL()],
#'   [ois_asset_swap_jpy_basis_GQL()], [make_asset_swap_GQL()] for the IBOR type.
#' @export
ois_asset_swap_GQL <- function(
  bond, clean_price, ois_curve, spread_bp = 0,
  payment_frequency = "Semiannual", payment_lag = 0L,
  index = ois_curve$overnight_index,
  discount_curve = ois_curve$curve,
  spread_day_counter = NULL
) {
  if (is.null(index)) stop("Supply a linked overnight index.")
  set_eval_date_GQL(date_GQL(ois_curve$evaluation_date))
  start_ql <- .quantlib_fun_GQH("Bond_settlementDate")(bond)
  start <- as.Date(safe_iso_GQH(start_ql))
  cf <- .ois_asw_table_GQL(.quantlib_fun_GQH("Bond_cashflows")(bond)) |>
    dplyr::filter(payment_date > start)
  if (!nrow(cf)) stop("No remaining bond cashflows.", call. = FALSE)
  maturity <- as.Date(safe_iso_GQH(.quantlib_fun_GQH("Bond_maturityDate")(bond)))
  nominal <- as.numeric(.quantlib_fun_GQH("Bond_notional")(bond, start_ql))
  if (abs(utils::tail(cf$amount, 1L) - nominal) > 1e-8 * nominal) {
    stop("Only bullet bonds with redemption at 100 are supported.", call. = FALSE)
  }
  before_dates <- unique(cf$payment_date[cf$payment_date < maturity])
  notionals <- purrr::map_dbl(before_dates, function(d) {
    as.numeric(.quantlib_fun_GQH("Bond_notional")(bond, date_GQL(as.Date(d, origin = "1970-01-01"))))
  })
  if (any(!is.finite(notionals)) || any(abs(notionals - nominal) > 1e-8 * nominal)) {
    stop("Amortising bonds are not supported.", call. = FALSE)
  }
  accrued <- as.numeric(.quantlib_fun_GQH("Bond_accruedAmount")(bond, start_ql))
  dirty <- clean_price + accrued
  if (is.null(spread_day_counter)) spread_day_counter <- index$dayCounter()
  frequency <- if (is.character(payment_frequency)) {
    .quantlib_fun_GQH(paste0("Frequency_", payment_frequency, "_get"))()
  } else {
    payment_frequency
  }
  builder <- .quantlib_fun_GQH("MakeOIS")(QuantLib::Period(1L, "Years"), index, 1)
  settings <- list(
    withEffectiveDate = start_ql,
    withTerminationDate = date_GQL(maturity),
    withNominal = nominal,
    withPaymentFrequency = frequency,
    withPaymentLag = as.integer(payment_lag),
    withFixedLegDayCount = spread_day_counter
  )
  builder <- purrr::reduce(names(settings), function(b, name) {
    .quantlib_fun_GQH(paste0("MakeOIS_", name))(b, settings[[name]])
  }, .init = builder)
  ois <- .quantlib_fun_GQH("MakeOIS_makeOIS")(builder)
  float <- .ois_asw_table_GQL(ois$overnightLeg())
  unit <- .ois_asw_table_GQL(ois$fixedLeg())
  if (!identical(float$payment_date, unit$payment_date) || any(unit$amount <= 0)) {
    stop("Fixed/overnight schedules differ; cannot compute spread annuity.", call. = FALSE)
  }
  float <- float |>
    dplyr::mutate(
      discount_factor = .ois_asw_discount_GQL(discount_curve, payment_date),
      spread_amount = unit$amount
    )
  cf <- cf |>
    dplyr::mutate(
      discount_factor = .ois_asw_discount_GQL(discount_curve, payment_date),
      present_value = amount * discount_factor
    )
  annuity <- sum(float$spread_amount * float$discount_factor)
  if (!is.finite(annuity) || annuity <= 0) stop("Invalid spread annuity.")

  spread <- spread_bp / 10000
  principal_date <- dplyr::last(float$payment_date)
  principal_df <- dplyr::last(float$discount_factor)
  start_df <- .ois_asw_discount_GQL(discount_curve, start)
  bond_pv <- sum(cf$present_value)
  float_pv <- sum(float$amount * float$discount_factor)
  principal_pv <- nominal * principal_df
  upfront <- nominal * (dirty / 100 - 1)
  upfront_pv <- upfront * start_df
  npv_zero <- float_pv + principal_pv + upfront_pv - bond_pv
  fair_spread <- -npv_zero / annuity

  signed_cf <- dplyr::bind_rows(
    cf |>
      dplyr::transmute(
        leg = "pay_bond", payment_date, amount = -amount, discount_factor
      ),
    float |>
      dplyr::transmute(
        leg = "receive_overnight", payment_date,
        amount = amount + spread * spread_amount, discount_factor
      ),
    tibble::tibble(
      leg = c("receive_principal", "upfront"),
      payment_date = c(principal_date, start),
      amount = c(nominal, upfront),
      discount_factor = c(principal_df, start_df)
    )
  ) |>
    dplyr::mutate(present_value = amount * discount_factor)
  structure(list(
    cashflows = signed_cf, bond = bond, ois = ois, index = index,
    forecast_handle = ois_curve$curve_handle, discount_curve = discount_curve,
    valuation = tibble::tibble(
      clean_price = clean_price, accrued_per_100 = accrued, dirty_price = dirty,
      nominal = nominal, spread = spread, fair_spread = fair_spread,
      fair_spread_bp = fair_spread * 1e4, spread_annuity = annuity,
      npv = npv_zero + spread * annuity,
      bond_leg_pv = -bond_pv, overnight_leg_pv = float_pv + spread * annuity,
      principal_pv = principal_pv, upfront_pv = upfront_pv
    ),
    price_sensitivity = nominal * start_df / 100,
    effective_date = start, maturity_date = maturity,
    payment_frequency = payment_frequency, payment_lag = payment_lag
  ), class = "ois_asset_swap_GQL")
}

#' Clean price implied by an OIS asset swap
#'
#' Clean price at which the asset swap, with its contract spread, has zero NPV.
#'
#' @param asset_swap Result of [ois_asset_swap_GQL()].
#'
#' @return Numeric clean price per 100.
#' @export
ois_asset_swap_fair_price_GQL <- function(asset_swap) {
  v <- asset_swap$valuation
  v$clean_price - v$npv / asset_swap$price_sensitivity
}

#' OIS asset swap spread by clean price
#'
#' Re-solves the fair spread of an [ois_asset_swap_GQL()] result for other
#' clean prices, keeping the curve and the bond unchanged.
#'
#' @param asset_swap Result of [ois_asset_swap_GQL()].
#' @param prices Numeric vector of clean prices per 100.
#'
#' @return A tibble with `clean_price`, `AS_spread_bp` and `AS_spread_pct`.
#' @export
ois_asset_swap_price_table_GQL <- function(asset_swap, prices) {
  if (!is.numeric(prices) || any(!is.finite(prices)) || any(prices <= 0)) {
    stop("prices must contain positive finite numbers.")
  }
  v <- asset_swap$valuation
  tibble::tibble(clean_price = prices) |>
    dplyr::mutate(
      AS_spread_pct = 100 * (
        v$fair_spread -
          (clean_price - v$clean_price) *
            asset_swap$price_sensitivity / v$spread_annuity
      ),
      AS_spread_bp = AS_spread_pct * 100
    ) |>
    dplyr::select(clean_price, AS_spread_bp, AS_spread_pct)
}

#' JPY-converted OIS asset swap from two zero-spread templates
#'
#' Core of [ois_asset_swap_jpy_basis_GQL()]. Both templates must be
#' zero-spread [ois_asset_swap_GQL()] results for the same bond: one on the
#' foreign OIS curve and one on the TONA curve.
#'
#' @param native_asw Zero-spread asset swap on the foreign OIS curve.
#' @param jpy_template Zero-spread asset swap on the JPY OIS curve.
#' @param foreign_curve Foreign OIS curve from [ir_build_ois_curve_GQH()].
#' @param jpy_curve JPY OIS curve from [ir_build_ois_curve_GQH()].
#' @param fx_spot JPY per foreign unit for exchange on the effective date.
#' @param basis_bp Cross-currency basis on the JPY leg in basis points.
#' @param currency Foreign currency code.
#' @param spread_bp Contract spread on the JPY leg in basis points.
#'
#' @return An object of class `ois_asset_swap_jpy_GQL`; see
#'   [ois_asset_swap_jpy_basis_GQL()].
#' @export
ois_asset_swap_jpy_from_legs_GQL <- function(
  native_asw, jpy_template, foreign_curve, jpy_curve, fx_spot, basis_bp,
  currency, spread_bp = 0
) {
  scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!scalar(fx_spot) || fx_spot <= 0 || !scalar(basis_bp) ||
      !scalar(spread_bp)) stop("Invalid FX or spread input.")
  if (toupper(currency) == "JPY") stop("Use a foreign currency.")
  if (!identical(as.Date(foreign_curve$evaluation_date),
                 as.Date(jpy_curve$evaluation_date))) stop("Curve dates differ.")
  if (!identical(as.Date(native_asw$effective_date),
                 as.Date(jpy_template$effective_date)) ||
      !identical(as.Date(native_asw$maturity_date),
                 as.Date(jpy_template$maturity_date))) stop("ASW dates differ.")
  nv <- native_asw$valuation
  jv <- jpy_template$valuation
  if (nrow(nv) != 1L || nrow(jv) != 1L ||
      nv$spread != 0 || jv$spread != 0 ||
      abs(nv$nominal - jv$nominal) > 1e-10) {
    stop("Use zero-spread templates with the same nominal.")
  }
  if (!scalar(jv$spread_annuity) || jv$spread_annuity <= 0) {
    stop("Invalid JPY annuity.")
  }
  start <- as.Date(native_asw$effective_date)
  eval_date <- as.Date(jpy_curve$evaluation_date)
  if (start < eval_date) stop("Effective date precedes valuation date.")
  time <- function(dates) as.numeric(as.Date(dates) - start) / 365
  foreign_start_df <- .ois_asw_discount_GQL(foreign_curve$curve, start)
  jpy_start_df <- .ois_asw_discount_GQL(jpy_curve$curve, start)
  fcf <- native_asw$cashflows
  jcf <- jpy_template$cashflows
  fbase <- fcf[fcf$leg %in% c("receive_overnight", "receive_principal"), ]
  jbase <- jcf[jcf$leg %in% c("receive_overnight", "receive_principal"), ]
  bond <- fcf[fcf$leg == "pay_bond", ]
  if (!nrow(fbase) || !nrow(jbase) || !nrow(bond) ||
      any(fbase$payment_date <= start) || any(jbase$payment_date <= start) ||
      any(bond$payment_date <= start)) stop("Missing or invalid future legs.")
  base_jpy_pv <- fx_spot * sum(
    jbase$amount * .ois_asw_discount_GQL(jpy_curve$curve, jbase$payment_date)
  )
  jpy_annuity <- fx_spot * jv$spread_annuity
  basis <- basis_bp / 1e4
  # Under the common JPY collateral convention:
  # F(t) = FX(start) * [Pf(t)/Pf(start)]/[Pj(t)/Pj(start)] * exp(-z*t).
  # Calibrate z so that the specified CCS is par, including final principal.
  foreign_base_pv <- function(z) fx_spot * jpy_start_df * sum(
    fbase$amount *
      .ois_asw_discount_GQL(foreign_curve$curve, fbase$payment_date) / foreign_start_df *
      exp(-z * time(fbase$payment_date))
  )
  objective <- function(z) {
    (foreign_base_pv(z) - base_jpy_pv - basis * jpy_annuity) /
      (fx_spot * nv$nominal)
  }
  z <- stats::uniroot(objective, c(-1, 1), tol = 1e-12)$root
  forward <- function(dates) {
    dates <- as.Date(dates)
    if (anyNA(dates) || any(dates < start)) stop("Invalid forward dates.")
    fx_spot *
      (.ois_asw_discount_GQL(foreign_curve$curve, dates) / foreign_start_df) /
      (.ois_asw_discount_GQL(jpy_curve$curve, dates) / jpy_start_df) *
      exp(-z * time(dates))
  }
  bond$foreign_amount <- -bond$amount
  bond$fx_forward <- forward(bond$payment_date)
  bond$jpy_amount <- bond$foreign_amount * bond$fx_forward
  bond$jpy_discount_factor <- .ois_asw_discount_GQL(jpy_curve$curve, bond$payment_date)
  bond$jpy_present_value <- bond$jpy_amount * bond$jpy_discount_factor
  bond_pv <- sum(bond$jpy_present_value)
  # Non-par bond price is settled at the initial FX exchange rate.
  upfront_pv <- fx_spot * jv$upfront_pv
  fair <- (bond_pv - base_jpy_pv - upfront_pv) / jpy_annuity
  npv <- base_jpy_pv + upfront_pv + spread_bp / 1e4 * jpy_annuity - bond_pv
  structure(list(
    valuation = tibble::tibble(
      currency = toupper(currency),
      native_asw_bp = nv$fair_spread_bp,
      basis_on_jpy_bp = basis_bp,
      jpy_asw_bp = fair * 1e4,
      simple_sum_bp = nv$fair_spread_bp + basis_bp,
      spread_bp = spread_bp,
      npv_jpy = npv,
      fair_spread_npv_jpy = base_jpy_pv + upfront_pv + fair * jpy_annuity - bond_pv,
      hedged_bond_pv_jpy = bond_pv,
      jpy_base_leg_pv = base_jpy_pv,
      upfront_pv_jpy = upfront_pv,
      spread_annuity_jpy = jpy_annuity,
      calibrated_discount_spread_bp = z * 1e4,
      ccs_repricing_npv_jpy = foreign_base_pv(z) - base_jpy_pv - basis * jpy_annuity
    ),
    hedged_bond_cashflows = bond,
    fx_forward = forward,
    native_asw = native_asw,
    jpy_template = jpy_template,
    effective_date = start,
    maturity_date = native_asw$maturity_date,
    assumptions = c(
      "JPY collateral; constant notionals; principal exchanged at start and end.",
      "Basis is on the JPY leg: pay foreign overnight / receive TONAR + basis.",
      "One maturity basis quote calibrates a constant foreign discount spread.",
      "Bond coupons and redemption are hedged using model FX forwards.",
      "Par-par ASW with initial price premium/discount exchanged at initial FX.",
      "No MtM resets, transaction costs, stochastic quanto or credit adjustments."
    )
  ), class = "ois_asset_swap_jpy_GQL")
}

#' JPY-converted OIS asset swap with a single basis quote
#'
#' Swaps a foreign fixed-rate bond into JPY with a JPY-collateralised,
#' constant-notional cross-currency swap and returns the asset swap spread
#' over TONA. The FX forward is
#' \deqn{F(t) = S \frac{P_f(t)/P_f(t_0)}{P_d(t)/P_d(t_0)} e^{-z (t - t_0)},}
#' where one constant `z` is calibrated so that the maturity cross-currency
#' swap with basis `basis_bp` on the JPY leg is at par.
#'
#' This is a simplified single-quote model: collateral is fixed in JPY (no
#' collateral choice / cheapest-to-deposit), there is no basis term structure
#' and no MtM notional reset, and no credit, funding or transaction-cost
#' adjustments. The returned `assumptions` element records these terms.
#'
#' @param bond QuantLib fixed-rate bullet bond in the foreign currency.
#' @param clean_price Clean price per 100.
#' @param foreign_curve Foreign OIS curve from [ir_build_ois_curve_GQH()].
#' @param jpy_curve JPY OIS curve from [ir_build_ois_curve_GQH()].
#' @param fx_spot JPY per foreign unit for exchange on the effective date.
#' @param basis_bp Cross-currency basis on the JPY leg in basis points
#'   (pay foreign overnight / receive TONA + basis).
#' @param currency Foreign currency code.
#' @param foreign_payment_frequency,jpy_payment_frequency Overnight leg
#'   payment frequencies.
#' @param foreign_payment_lag,jpy_payment_lag Overnight leg payment lags.
#' @param spread_bp Contract spread on the JPY leg in basis points.
#'
#' @return An object of class `ois_asset_swap_jpy_GQL`: a list with
#'   `valuation` (one-row tibble with `native_asw_bp`, `jpy_asw_bp`,
#'   `simple_sum_bp`, `calibrated_discount_spread_bp`, `ccs_repricing_npv_jpy`,
#'   `fair_spread_npv_jpy`, ...), `hedged_bond_cashflows`, the `fx_forward`
#'   function and `assumptions`.
#' @seealso [ois_asset_swap_GQL()], [ois_asset_swap_jpy_from_legs_GQL()].
#' @export
ois_asset_swap_jpy_basis_GQL <- function(
  bond, clean_price, foreign_curve, jpy_curve, fx_spot, basis_bp,
  currency = foreign_curve$currency,
  foreign_payment_frequency = "Semiannual",
  jpy_payment_frequency = "Semiannual",
  foreign_payment_lag = 0L, jpy_payment_lag = 0L, spread_bp = 0
) {
  if (is.null(currency) || length(currency) != 1L || is.na(currency)) {
    stop("Supply the foreign currency.")
  }
  if (!is.null(jpy_curve$currency) && jpy_curve$currency != "JPY") {
    stop("jpy_curve must be JPY.")
  }
  if (!is.null(foreign_curve$currency) &&
      toupper(currency) != foreign_curve$currency) stop("Foreign currency mismatch.")
  native <- ois_asset_swap_GQL(
    bond, clean_price, foreign_curve, spread_bp = 0,
    payment_frequency = foreign_payment_frequency, payment_lag = foreign_payment_lag
  )
  yen <- ois_asset_swap_GQL(
    bond, clean_price, jpy_curve, spread_bp = 0,
    payment_frequency = jpy_payment_frequency, payment_lag = jpy_payment_lag
  )
  ois_asset_swap_jpy_from_legs_GQL(
    native, yen, foreign_curve, jpy_curve, fx_spot, basis_bp, currency, spread_bp
  )
}
