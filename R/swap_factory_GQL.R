# swap_factory.R

#' Create an OIS swap
#'
#' @param swap_tenor QuantLib Period object.
#' @param overnight_index QuantLib overnight index object.
#' @param fixed_rate Fixed rate.
#'
#' @return A QuantLib OIS swap object.
#' @export
make_ois_GQL <- function(
    swap_tenor,
    overnight_index,
    fixed_rate
) {
  qlg_use_quantlib()

  swap_builder <- QuantLib::MakeOIS(
    swapTenor = swap_tenor,
    overnightIndex = overnight_index,
    fixedRate = fixed_rate
  )

  QuantLib::MakeOIS_makeOIS(swap_builder)
}

#' Create an Eonia OIS swap
#'
#' @param swap_tenor QuantLib Period object.
#' @param forecast_handle QuantLib yield term structure handle.
#' @param fixed_rate Fixed rate.
#'
#' @return A QuantLib OIS swap object.
#' @export
make_eonia_ois_GQL <- function(
    swap_tenor,
    forecast_handle,
    fixed_rate
) {
  qlg_use_quantlib()

  overnight_index <- QuantLib::Eonia(forecast_handle)

  make_ois_GQL(
    swap_tenor = swap_tenor,
    overnight_index = overnight_index,
    fixed_rate = fixed_rate
  )
}

# R/swap_factory.R






# R/swap_factory.R





#' Make an OIS from trade data
#'
#' @param trade A one-row data frame containing OIS trade fields.
#' @param forecast_handle QuantLib forecast curve handle.
#'
#' @return A QuantLib OIS object.
#'
#' @export
make_ois_from_trade_GQL <- function(
    trade,
    forecast_handle
) {
  qlg_use_quantlib()

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  index <- toupper(
    as.character(
      trade_value_GQL(
        trade = trade,
        name = "index",
        default = "EONIA"
      )
    )
  )

  swap_tenor <- trade_period_GQL(
    trade = trade,
    n_col = "tenor_n",
    unit_col = "tenor_unit",
    default_n = 5,
    default_unit = "Years"
  )

  fixed_rate <- as.numeric(
    trade_value_GQL(
      trade = trade,
      name = "fixed_rate",
      default = 0
    )
  )

  if (index == "EONIA") {
    return(
      make_eonia_ois_GQL(
        swap_tenor = swap_tenor,
        forecast_handle = forecast_handle,
        fixed_rate = fixed_rate
      )
    )
  }

  stop(
    "Unsupported OIS index: ",
    index,
    ". Currently supported: EONIA."
  )
}

# R/swap_factory.R

vanilla_swap_type_GQL <- function(type = "payer") {
  type <- tolower(as.character(type))

  if (type %in% c("payer", "pay", "pay_fixed", "fixed_payer")) {
    return("Payer")
  }

  if (type %in% c("receiver", "receive", "receive_fixed", "fixed_receiver")) {
    return("Receiver")
  }

  stop(
    "Unsupported swap type: ",
    type,
    ". Use 'payer' or 'receiver'."
  )
}


make_ibor_index_GQL <- function(
    index = "Euribor6M",
    forecast_handle = NULL
) {
  qlg_use_quantlib()

  index <- toupper(as.character(index))

  if (index %in% c("EURIBOR6M", "EURIBOR_6M")) {
    if (is.null(forecast_handle)) {
      return(QuantLib::Euribor6M())
    }

    return(QuantLib::Euribor6M(forecast_handle))
  }

  if (index %in% c("EURIBOR3M", "EURIBOR_3M")) {
    tenor <- QuantLib::Period(3, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::Euribor(tenor))
    }

    return(QuantLib::Euribor(tenor, forecast_handle))
  }

  if (index %in% c("USD_LIBOR_3M", "USDLIBOR3M", "LIBOR3M")) {
    tenor <- QuantLib::Period(3, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::USDLibor(tenor))
    }

    return(QuantLib::USDLibor(tenor, forecast_handle))
  }

  if (index %in% c("USD_LIBOR_6M", "USDLIBOR6M", "LIBOR6M")) {
    tenor <- QuantLib::Period(6, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::USDLibor(tenor))
    }

    return(QuantLib::USDLibor(tenor, forecast_handle))
  }

  stop(
    "Unsupported ibor index: ",
    index,
    ". Currently supported: Euribor6M, Euribor3M, USDLibor3M, USDLibor6M."
  )
}


make_schedule_GQL <- function(
    effective_date,
    maturity_date,
    tenor = QuantLib::Period(1, "Years"),
    calendar = QuantLib::TARGET(),
    convention = "ModifiedFollowing",
    termination_convention = "ModifiedFollowing",
    date_generation = "Forward",
    end_of_month = FALSE
) {
  qlg_use_quantlib()

  QuantLib::Schedule(
    qlg_date(effective_date),
    qlg_date(maturity_date),
    tenor,
    calendar,
    convention,
    termination_convention,
    QuantLib::copyToR(
      QuantLib::DateGeneration(),
      date_generation
    ),
    end_of_month
  )
}


#' Make a QuantLib VanillaSwap
#'
#' @param effective_date Effective date.
#' @param maturity_date Maturity date.
#' @param fixed_rate Fixed coupon rate.
#' @param notional Notional amount.
#' @param forecast_handle Forecast curve handle.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#' @param swap_type Payer or receiver.
#' @param index Floating index name.
#' @param spread Floating spread.
#' @param fixed_tenor Fixed leg tenor.
#' @param floating_tenor Floating leg tenor.
#' @param fixed_day_counter Fixed leg day counter.
#' @param floating_day_counter Floating leg day counter.
#' @param calendar QuantLib calendar.
#' @param fixed_convention Fixed leg business day convention.
#' @param floating_convention Floating leg business day convention.
#'
#' @return A QuantLib VanillaSwap object.
#'
#' @export
make_vanilla_swap_GQL <- function(
    effective_date,
    maturity_date,
    fixed_rate,
    notional = 1,
    forecast_handle = NULL,
    discount_handle = NULL,
    swap_type = "payer",
    index = "Euribor6M",
    spread = 0,
    fixed_tenor = QuantLib::Period(1, "Years"),
    floating_tenor = QuantLib::Period(6, "Months"),
    fixed_day_counter = QuantLib::Thirty360("European"),
    floating_day_counter = QuantLib::Actual360(),
    calendar = QuantLib::TARGET(),
    fixed_convention = "ModifiedFollowing",
    floating_convention = "ModifiedFollowing"
) {
  qlg_use_quantlib()

  fixed_schedule <- make_schedule_GQL(
    effective_date = effective_date,
    maturity_date = maturity_date,
    tenor = fixed_tenor,
    calendar = calendar,
    convention = fixed_convention,
    termination_convention = fixed_convention
  )

  floating_schedule <- make_schedule_GQL(
    effective_date = effective_date,
    maturity_date = maturity_date,
    tenor = floating_tenor,
    calendar = calendar,
    convention = floating_convention,
    termination_convention = floating_convention
  )

  floating_index <- make_ibor_index_GQL(
    index = index,
    forecast_handle = forecast_handle
  )

  swap <- QuantLib::VanillaSwap(
    vanilla_swap_type_GQL(swap_type),
    as.numeric(notional),
    fixed_schedule,
    as.numeric(fixed_rate),
    fixed_day_counter,
    floating_schedule,
    floating_index,
    as.numeric(spread),
    floating_day_counter
  )

  if (!is.null(discount_handle)) {
    engine <- QuantLib::DiscountingSwapEngine(discount_handle)
    swap$setPricingEngine(engine)
  }

  swap
}


trade_value_GQL <- function(
    trade,
    name,
    default = NULL
) {
  stopifnot(is.data.frame(trade))

  if (!(name %in% names(trade))) {
    return(default)
  }

  value <- trade[[name]][[1]]

  if (length(value) == 0 || is.na(value)) {
    default
  } else {
    value
  }
}


trade_period_GQL <- function(
    trade,
    n_col,
    unit_col,
    default_n,
    default_unit
) {
  qlg_use_quantlib()

  n <- trade_value_GQL(
    trade = trade,
    name = n_col,
    default = default_n
  )

  unit <- trade_value_GQL(
    trade = trade,
    name = unit_col,
    default = default_unit
  )

  QuantLib::Period(
    as.integer(n),
    as.character(unit)
  )
}


#' Make a QuantLib VanillaSwap from trade data
#'
#' @param trade A one-row data frame containing VanillaSwap trade fields.
#' @param forecast_handle Forecast curve handle.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#'
#' @return A QuantLib VanillaSwap object.
#'
#' @export
make_vanilla_swap_from_trade_GQL <- function(
    trade,
    forecast_handle = NULL,
    discount_handle = NULL
) {
  qlg_use_quantlib()

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  effective_date <- trade_value_GQL(
    trade = trade,
    name = "effective_date",
    default = NULL
  )

  maturity_date <- trade_value_GQL(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(effective_date) || is.null(maturity_date)) {
    stop("trade must contain effective_date and maturity_date.")
  }

  fixed_tenor <- trade_period_GQL(
    trade = trade,
    n_col = "fixed_tenor_n",
    unit_col = "fixed_tenor_unit",
    default_n = 1,
    default_unit = "Years"
  )

  floating_tenor <- trade_period_GQL(
    trade = trade,
    n_col = "floating_tenor_n",
    unit_col = "floating_tenor_unit",
    default_n = 6,
    default_unit = "Months"
  )

  make_vanilla_swap_GQL(
    effective_date = as.character(effective_date),
    maturity_date = as.character(maturity_date),
    fixed_rate = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "fixed_rate",
        default = 0
      )
    ),
    notional = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "notional",
        default = 1
      )
    ),
    forecast_handle = forecast_handle,
    discount_handle = discount_handle,
    swap_type = trade_value_GQL(
      trade = trade,
      name = "swap_type",
      default = "payer"
    ),
    index = trade_value_GQL(
      trade = trade,
      name = "index",
      default = "Euribor6M"
    ),
    spread = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "spread",
        default = 0
      )
    ),
    fixed_tenor = fixed_tenor,
    floating_tenor = floating_tenor
  )
}


.qlg_quantlib_fun <- function(name) {
  ql <- asNamespace("QuantLib")

  if (!exists(name, envir = ql, inherits = FALSE)) {
    stop("QuantLib function is not available: ", name, call. = FALSE)
  }

  get(name, envir = ql, inherits = FALSE)
}

#' Build a QuantLib AssetSwap
#'
#' This is a thin factory wrapper around QuantLib AssetSwap.
#' The floating index can be supplied either as a QuantLib IborIndex object
#' or as a supported index name such as "Euribor6M".
#'
#' @param bond QuantLib bond object.
#' @param clean_price Bond clean price.
#' @param floating_schedule QuantLib Schedule object for the floating leg.
#' @param index Floating index name or QuantLib IborIndex object.
#' @param forecast_handle Forecast curve handle used when index is supplied as a character name.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#' @param floating_day_counter Floating leg day counter name or QuantLib DayCounter object.
#' @param maturity_date Optional maturity date as character or Date.
#' @param deal_maturity Optional QuantLib Date object for the AssetSwap deal maturity.
#' @param pay_bond_coupon Logical. TRUE means paying the bond coupon leg.
#' @param spread Floating leg spread.
#' @param par_asset_swap Logical. TRUE for par asset swap.
#' @param gearing Floating leg gearing.
#' @param non_par_repayment Non-par repayment amount.
#'
#' @return QuantLib AssetSwap object.
#'
#' @export
make_asset_swap_GQL <- function(
    bond,
    clean_price,
    floating_schedule,
    index = "Euribor6M",
    forecast_handle = NULL,
    discount_handle = NULL,
    floating_day_counter = "Actual360",
    maturity_date = NULL,
    deal_maturity = NULL,
    pay_bond_coupon = TRUE,
    spread = 0,
    par_asset_swap = TRUE,
    gearing = 1,
    non_par_repayment = 100
) {
  qlg_use_quantlib()

  if (missing(floating_schedule) || is.null(floating_schedule)) {
    stop("floating_schedule must be supplied.", call. = FALSE)
  }

  ibor_index <- if (is.character(index)) {
    make_ibor_index_GQL(
      index = index,
      forecast_handle = forecast_handle
    )
  } else {
    index
  }

  floating_day_counter <- qlg_day_counter(floating_day_counter)

  if (is.null(deal_maturity)) {
    if (!is.null(maturity_date)) {
      deal_maturity <- qlg_date(maturity_date)
    } else if (exists("Bond_maturityDate", envir = asNamespace("QuantLib"), inherits = FALSE)) {
      deal_maturity <- .qlg_quantlib_fun("Bond_maturityDate")(bond)
    } else {
      stop(
        "Either maturity_date or deal_maturity must be supplied.",
        call. = FALSE
      )
    }
  } else if (is.character(deal_maturity) || inherits(deal_maturity, "Date")) {
    deal_maturity <- qlg_date(deal_maturity)
  }

  asset_swap <- .qlg_quantlib_fun("AssetSwap__SWIG_0")(
    as.logical(pay_bond_coupon),
    bond,
    as.numeric(clean_price),
    ibor_index,
    as.numeric(spread),
    floating_schedule,
    floating_day_counter,
    as.logical(par_asset_swap),
    as.numeric(gearing),
    as.numeric(non_par_repayment),
    deal_maturity
  )

  if (!is.null(discount_handle)) {
    asset_swap_set_engine_GQL(
      asset_swap = asset_swap,
      discount_curve_handle = discount_handle
    )
  }

  asset_swap
}

.qlg_asset_swap_set_pricing_engine <- function(asset_swap, engine) {
  .qlg_quantlib_fun("Instrument_setPricingEngine")(asset_swap, engine)
  invisible(asset_swap)
}

#' Attach a discounting engine to an AssetSwap
#'
#' @param asset_swap QuantLib AssetSwap object.
#' @param discount_curve_handle QuantLib YieldTermStructureHandle.
#'
#' @return The original AssetSwap object, invisibly.
#' @export
asset_swap_set_engine_GQL <- function(asset_swap, discount_curve_handle) {
  requireNamespace("QuantLib", quietly = TRUE)

  engine <- .qlg_quantlib_fun("DiscountingSwapEngine")(discount_curve_handle)
  .qlg_asset_swap_set_pricing_engine(asset_swap, engine)

  invisible(asset_swap)
}

.qlg_asset_swap_value <- function(asset_swap, fun_name) {
  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(asset_swap),
    error = function(e) NA_real_
  )

  as.numeric(out)
}

#' AssetSwap fair spread
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return Numeric fair spread.
#' @export
asset_swap_fair_spread_GQL <- function(asset_swap) {
  .qlg_asset_swap_value(asset_swap, "AssetSwap_fairSpread")
}

#' AssetSwap fair clean price
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return Numeric fair clean price.
#' @export
asset_swap_fair_clean_price_GQL <- function(asset_swap) {
  .qlg_asset_swap_value(asset_swap, "AssetSwap_fairCleanPrice")
}

#' Summarise an AssetSwap
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return A tibble with AssetSwap analytics.
#' @export
asset_swap_summary_GQL <- function(asset_swap) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c(
      "fair_spread",
      "fair_clean_price"
    ),
    value = c(
      asset_swap_fair_spread_GQL(asset_swap),
      asset_swap_fair_clean_price_GQL(asset_swap)
    )
  )
}


.qlg_trade_logical <- function(
    trade,
    name,
    default = FALSE
) {
  value <- trade_value_GQL(
    trade = trade,
    name = name,
    default = default
  )

  if (is.logical(value)) {
    return(value)
  }

  if (is.numeric(value)) {
    return(value != 0)
  }

  value_chr <- tolower(as.character(value))

  if (value_chr %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }

  if (value_chr %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }

  stop(
    "Could not convert trade field '",
    name,
    "' to logical: ",
    value,
    call. = FALSE
  )
}

#' Make a QuantLib AssetSwap from trade data
#'
#' This builds an AssetSwap from a one-row trade data frame and a QuantLib bond.
#' If a floating schedule is not supplied, it is built from effective_date,
#' maturity_date, and floating_tenor fields in the trade.
#'
#' @param trade A one-row data frame containing AssetSwap trade fields.
#' @param bond QuantLib bond object.
#' @param forecast_handle Forecast curve handle used when index is supplied as a character name.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#' @param floating_schedule Optional QuantLib Schedule object for the floating leg.
#' @param calendar QuantLib calendar used when building the floating schedule.
#'
#' @return A QuantLib AssetSwap object.
#'
#' @export
make_asset_swap_from_trade_GQL <- function(
    trade,
    bond,
    forecast_handle = NULL,
    discount_handle = NULL,
    floating_schedule = NULL,
    calendar = QuantLib::TARGET()
) {
  qlg_use_quantlib()

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  clean_price <- as.numeric(
    trade_value_GQL(
      trade = trade,
      name = "clean_price",
      default = 100
    )
  )

  effective_date <- trade_value_GQL(
    trade = trade,
    name = "effective_date",
    default = NULL
  )

  maturity_date <- trade_value_GQL(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(floating_schedule)) {
    if (is.null(effective_date) || is.null(maturity_date)) {
      stop(
        "trade must contain effective_date and maturity_date ",
        "when floating_schedule is not supplied.",
        call. = FALSE
      )
    }

    floating_tenor <- trade_period_GQL(
      trade = trade,
      n_col = "floating_tenor_n",
      unit_col = "floating_tenor_unit",
      default_n = 6,
      default_unit = "Months"
    )

    floating_schedule <- make_schedule_GQL(
      effective_date = as.character(effective_date),
      maturity_date = as.character(maturity_date),
      tenor = floating_tenor,
      calendar = calendar,
      convention = trade_value_GQL(
        trade = trade,
        name = "floating_convention",
        default = "ModifiedFollowing"
      ),
      termination_convention = trade_value_GQL(
        trade = trade,
        name = "floating_termination_convention",
        default = "ModifiedFollowing"
      ),
      date_generation = trade_value_GQL(
        trade = trade,
        name = "floating_date_generation",
        default = "Forward"
      ),
      end_of_month = .qlg_trade_logical(
        trade = trade,
        name = "floating_end_of_month",
        default = FALSE
      )
    )
  }

  make_asset_swap_GQL(
    bond = bond,
    clean_price = clean_price,
    floating_schedule = floating_schedule,
    index = trade_value_GQL(
      trade = trade,
      name = "index",
      default = "Euribor6M"
    ),
    forecast_handle = forecast_handle,
    discount_handle = discount_handle,
    floating_day_counter = trade_value_GQL(
      trade = trade,
      name = "floating_day_counter",
      default = "Actual360"
    ),
    maturity_date = maturity_date,
    pay_bond_coupon = .qlg_trade_logical(
      trade = trade,
      name = "pay_bond_coupon",
      default = TRUE
    ),
    spread = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "spread",
        default = 0
      )
    ),
    par_asset_swap = .qlg_trade_logical(
      trade = trade,
      name = "par_asset_swap",
      default = TRUE
    ),
    gearing = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "gearing",
        default = 1
      )
    ),
    non_par_repayment = as.numeric(
      trade_value_GQL(
        trade = trade,
        name = "non_par_repayment",
        default = 100
      )
    )
  )
}

#' Build a QuantLib CreditDefaultSwap
#'
#' This is a thin factory wrapper around QuantLib MakeCreditDefaultSwap.
#' It builds the CDS instrument only. Pricing requires a pricing engine.
#'
#' @param maturity_date CDS maturity date as character, Date, or QuantLib Date.
#' @param running_spread Running CDS spread.
#' @param notional CDS notional.
#' @param side Protection side. Use "buyer", "seller", or a QuantLib Protection object.
#' @param trade_date Optional trade date as character, Date, or QuantLib Date.
#' @param coupon_tenor Coupon tenor. Defaults to "3M".
#' @param day_counter Day counter name or QuantLib DayCounter object.
#' @param date_generation_rule Date generation rule. Defaults to "CDS2015".
#' @param pricing_engine Optional QuantLib pricing engine.
#' @param upfront_rate Optional upfront rate.
#'
#' @return QuantLib CreditDefaultSwap object.
#' @export
make_cds_GQL <- function(
    maturity_date,
    running_spread,
    notional = 1,
    side = "buyer",
    trade_date = NULL,
    coupon_tenor = "3M",
    day_counter = "Actual360",
    date_generation_rule = "CDS2015",
    pricing_engine = NULL,
    upfront_rate = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  maturity_date <- .qlg_cds_date(maturity_date)
  coupon_tenor <- .qlg_cds_period(coupon_tenor)
  side <- .qlg_cds_side(side)
  day_counter <- .qlg_cds_day_counter(day_counter)
  date_generation_rule <- .qlg_cds_date_generation_rule(date_generation_rule)

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap__SWIG_1")(
    maturity_date,
    as.numeric(running_spread)
  )

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withNominal")(
    builder,
    as.numeric(notional)
  )

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withSide")(
    builder,
    side
  )

  if (!is.null(trade_date)) {
    builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withTradeDate")(
      builder,
      .qlg_cds_date(trade_date)
    )
  }

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withCouponTenor")(
    builder,
    coupon_tenor
  )

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withDayCounter")(
    builder,
    day_counter
  )

  builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withDateGenerationRule")(
    builder,
    date_generation_rule
  )

  if (!is.null(upfront_rate)) {
    builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withUpfrontRate")(
      builder,
      as.numeric(upfront_rate)
    )
  }

  if (!is.null(pricing_engine)) {
    builder <- .qlg_quantlib_fun("MakeCreditDefaultSwap_withPricingEngine")(
      builder,
      pricing_engine
    )
  }

  .qlg_quantlib_fun("MakeCreditDefaultSwap_makeCDS")(builder)
}

#' Make a QuantLib CreditDefaultSwap from trade data
#'
#' @param trade A one-row data frame containing CDS trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib CreditDefaultSwap object.
#' @export
make_cds_from_trade_GQL <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  make_cds_GQL(
    maturity_date = trade_value_GQL(
      trade = trade,
      name = "maturity_date",
      default = NULL
    ),
    running_spread = trade_value_GQL(
      trade = trade,
      name = "running_spread",
      default = NULL
    ),
    notional = trade_value_GQL(
      trade = trade,
      name = "notional",
      default = 1
    ),
    side = trade_value_GQL(
      trade = trade,
      name = "side",
      default = "buyer"
    ),
    trade_date = trade_value_GQL(
      trade = trade,
      name = "trade_date",
      default = NULL
    ),
    coupon_tenor = trade_value_GQL(
      trade = trade,
      name = "coupon_tenor",
      default = "3M"
    ),
    day_counter = trade_value_GQL(
      trade = trade,
      name = "day_counter",
      default = "Actual360"
    ),
    date_generation_rule = trade_value_GQL(
      trade = trade,
      name = "date_generation_rule",
      default = "CDS2015"
    ),
    pricing_engine = pricing_engine,
    upfront_rate = trade_value_GQL(
      trade = trade,
      name = "upfront_rate",
      default = NULL
    )
  )
}

#' CDS NPV
#'
#' @param cds QuantLib CreditDefaultSwap object.
#'
#' @return Numeric NPV.
#' @export
cds_npv_GQL <- function(cds) {
  .qlg_cds_value(cds, "Instrument_NPV")
}

#' CDS fair spread
#'
#' @param cds QuantLib CreditDefaultSwap object.
#'
#' @return Numeric fair spread.
#' @export
cds_fair_spread_GQL <- function(cds) {
  .qlg_cds_value(cds, "CreditDefaultSwap_fairSpread")
}

#' Summarise a CDS
#'
#' @param cds QuantLib CreditDefaultSwap object.
#'
#' @return A tibble with CDS analytics.
#' @export
cds_summary_GQL <- function(cds) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c(
      "npv",
      "fair_spread",
      "coupon_leg_npv",
      "default_leg_npv"
    ),
    value = c(
      cds_npv_GQL(cds),
      cds_fair_spread_GQL(cds),
      .qlg_cds_value(cds, "CreditDefaultSwap_couponLegNPV"),
      .qlg_cds_value(cds, "CreditDefaultSwap_defaultLegNPV")
    )
  )
}

.qlg_cds_value <- function(cds, fun_name) {
  requireNamespace("QuantLib", quietly = TRUE)

  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(cds),
    error = function(e) {
      stop(
        "Failed to calculate CDS value with ", fun_name,
        ". A pricing engine may be required.",
        call. = FALSE
      )
    }
  )

  as.numeric(out)
}

.qlg_cds_date <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.character(x) || inherits(x, "Date")) {
    return(qlg_date(as.character(x)))
  }

  x
}

.qlg_cds_period <- function(x) {
  if (inherits(x, "_p_Period")) {
    return(x)
  }

  x <- toupper(trimws(as.character(x)))

  match <- regexec("^([0-9]+)\\s*([DWMY])$", x)
  parts <- regmatches(x, match)[[1]]

  if (length(parts) != 3) {
    stop("Unsupported CDS period: ", x, call. = FALSE)
  }

  n <- as.integer(parts[[2]])
  unit <- parts[[3]]

  if (identical(unit, "D")) {
    return(QuantLib::Period(n, QuantLib::TimeUnit_Days_get()))
  }

  if (identical(unit, "W")) {
    return(QuantLib::Period(n, QuantLib::TimeUnit_Weeks_get()))
  }

  if (identical(unit, "M")) {
    return(QuantLib::Period(n, QuantLib::TimeUnit_Months_get()))
  }

  if (identical(unit, "Y")) {
    return(QuantLib::Period(12L * n, QuantLib::TimeUnit_Months_get()))
  }

  stop("Unsupported CDS period: ", x, call. = FALSE)
}

.qlg_cds_side <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(trimws(x))

  if (x %in% c("buyer", "buy", "protection_buyer")) {
    return(QuantLib::Protection_Buyer_get())
  }

  if (x %in% c("seller", "sell", "protection_seller")) {
    return(QuantLib::Protection_Seller_get())
  }

  stop("Unsupported CDS side: ", x, call. = FALSE)
}

.qlg_cds_day_counter <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(gsub("[^A-Za-z0-9]", "", trimws(x)))

  if (x %in% c("actual360", "act360")) {
    return(QuantLib::Actual360())
  }

  if (x %in% c("actual365fixed", "act365fixed", "actual365", "act365")) {
    return(QuantLib::Actual365Fixed())
  }

  stop("Unsupported CDS day counter: ", x, call. = FALSE)
}

.qlg_cds_date_generation_rule <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- toupper(gsub("[^A-Za-z0-9]", "", trimws(x)))

  if (identical(x, "CDS2015")) {
    return(QuantLib::DateGeneration_CDS2015_get())
  }

  if (identical(x, "CDS")) {
    return(QuantLib::DateGeneration_CDS_get())
  }

  if (identical(x, "OLDCDS")) {
    return(QuantLib::DateGeneration_OldCDS_get())
  }

  stop("Unsupported CDS date generation rule: ", x, call. = FALSE)
}

#' Build a flat hazard-rate default probability handle
#'
#' This is a small helper around QuantLib FlatHazardRate and
#' DefaultProbabilityTermStructureHandle.
#'
#' @param hazard_rate Flat hazard rate.
#' @param reference_date Reference date as character, Date, or QuantLib Date.
#' @param day_counter Day counter name or QuantLib DayCounter object.
#'
#' @return QuantLib DefaultProbabilityTermStructureHandle.
#' @export
flat_hazard_rate_GQL <- function(
    hazard_rate,
    reference_date,
    day_counter = "Actual365Fixed"
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  reference_date <- .qlg_cds_date(reference_date)
  day_counter <- .qlg_cds_day_counter(day_counter)

  hazard_curve <- .qlg_quantlib_fun("FlatHazardRate__SWIG_1")(
    reference_date,
    qlg_quote_handle(as.numeric(hazard_rate)),
    day_counter
  )

  .qlg_quantlib_fun("DefaultProbabilityTermStructureHandle__SWIG_2")(
    hazard_curve
  )
}

#' Build a MidPoint CDS pricing engine
#'
#' This builds a QuantLib MidPointCdsEngine. You may supply existing
#' probability and discount handles, or supply flat hazard and discount rates.
#'
#' @param probability_handle Optional QuantLib DefaultProbabilityTermStructureHandle.
#' @param recovery_rate Recovery rate.
#' @param discount_handle Optional QuantLib YieldTermStructureHandle.
#' @param hazard_rate Optional flat hazard rate used when probability_handle is NULL.
#' @param discount_rate Optional flat discount rate used when discount_handle is NULL.
#' @param reference_date Reference date used for flat curves.
#' @param day_counter Day counter name or QuantLib DayCounter object.
#'
#' @return QuantLib MidPointCdsEngine object.
#' @export
cds_midpoint_engine_GQL <- function(
    probability_handle = NULL,
    recovery_rate = 0.40,
    discount_handle = NULL,
    hazard_rate = NULL,
    discount_rate = NULL,
    reference_date = NULL,
    day_counter = "Actual365Fixed"
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  if (is.null(probability_handle)) {
    if (is.null(hazard_rate) || is.null(reference_date)) {
      stop(
        "Either probability_handle or both hazard_rate and reference_date must be supplied.",
        call. = FALSE
      )
    }

    probability_handle <- flat_hazard_rate_GQL(
      hazard_rate = hazard_rate,
      reference_date = reference_date,
      day_counter = day_counter
    )
  }

  if (is.null(discount_handle)) {
    if (is.null(discount_rate) || is.null(reference_date)) {
      stop(
        "Either discount_handle or both discount_rate and reference_date must be supplied.",
        call. = FALSE
      )
    }

    reference_date <- .qlg_cds_date(reference_date)
    day_counter <- .qlg_cds_day_counter(day_counter)

    discount_curve <- .qlg_quantlib_fun("FlatForward__SWIG_2")(
      reference_date,
      qlg_quote_handle(as.numeric(discount_rate)),
      day_counter
    )

    discount_handle <- .qlg_quantlib_fun("YieldTermStructureHandle__SWIG_2")(
      discount_curve
    )
  }

  .qlg_quantlib_fun("MidPointCdsEngine")(
    probability_handle,
    as.numeric(recovery_rate),
    discount_handle
  )
}