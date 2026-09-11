# interest_rate_curves_audit_GQL_v3.R
#
# Source of truth:
# besuke/LagrangeFinance
# part2_quantive_riemann/ch02_interest_rate_curves.qmd
#
# QuantLib Python Cookbook chapter:
# Interest-rate curves
#
# Purpose:
# - Keep the whole chapter in one file for overview.
# - Compare Cookbook recipes with the existing LagrangeFinance implementation.
# - Identify which operations are already wrapped by GaussQuant.
# - Identify remaining raw QuantLib calls and wrapper candidates.
#
# Naming convention:
# - obj_ : QuantLib or other external objects
# - tbl_ : tibble
# - vec_ : vectors
# - lst_ : lists
# - str_ : character scalars
# - num_ : numeric scalars
# - int_ : integer scalars
# - flg_ : logical scalars

library(tidyverse)
library(QuantLib)

devtools::load_all()

# =============================================================================
# Chapter-level mapping
# =============================================================================

tbl_recipe_mapping <- tibble::tribble(
  ~int_recipe_no,
  ~str_cookbook_recipe,
  ~str_lagrange_section,
  ~str_status,
  ~str_primary_gaussquant_functions,

  1L,
  "EONIA curve bootstrapping",
  "2.3.2 Deposit and swap bootstrap",
  "partially_covered",
  paste(
    "build_swap_curve_GQL",
    "yield_curve_handle_GQL",
    "curve_tbl_GQH",
    "curve_grid_tbl_GQL",
    sep = "; "
  ),

  2L,
  "Euribor curve bootstrapping",
  "2.3.2 Deposit and swap bootstrap",
  "covered_at_high_level",
  paste(
    "build_swap_curve_GQL",
    "make_euribor6m_GQL",
    "yield_curve_handle_GQL",
    "curve_tbl_GQH",
    sep = "; "
  ),

  3L,
  "Constructing a yield curve",
  paste(
    "2.1 Interest-rate curve basics",
    "2.2 Interpolation and reference date",
    "2.3 Bootstrapping",
    sep = " / "
  ),
  "covered",
  paste(
    "build_zero_curve_GQL",
    "build_discount_curve_GQL",
    "build_bond_discount_curve_GQL",
    "build_swap_curve_GQL",
    "curve_tbl_GQH",
    "curve_grid_tbl_GQL",
    sep = "; "
  ),

  4L,
  "Dangerous day-count conventions",
  "2.3.1 Deposit and bond bootstrap",
  "partially_covered",
  paste(
    "day_counter_GQL",
    "build_bond_discount_curve_GQL",
    sep = "; "
  ),

  5L,
  "Implied term structures",
  "2.4.1 Implied Term Structure",
  "partially_covered",
  paste(
    "date_GQL",
    "curve_tbl_GQH",
    sep = "; "
  ),

  6L,
  "Interest-rate sensitivities via zero spread",
  "2.4.2 Parallel shift",
  "covered",
  paste(
    "zero_spreaded_term_structure_GQL",
    "curve_tbl_GQH",
    "curve_grid_tbl_GQL",
    sep = "; "
  ),

  7L,
  "A glitch in forward-rate curves",
  "2.1.3 Curve representation / 2.2.1 Interpolation comparison",
  "covered_conceptually",
  paste(
    "curve_grid_tbl_GQL",
    "curve_discount_safe_GQL",
    sep = "; "
  )
)

tbl_recipe_mapping

# =============================================================================
# Existing GaussQuant coverage found in LagrangeFinance
# =============================================================================

tbl_existing_wrappers <- tibble::tribble(
  ~str_function,
  ~str_role,
  ~str_cookbook_area,

  "set_eval_date_GQL",
  "Set QuantLib evaluation date",
  "All recipes",

  "date_GQL",
  "Convert ISO text to QuantLib Date",
  "Reference dates and scenario dates",

  "build_zero_curve_GQL",
  "Build a ZeroCurve from date/rate nodes",
  "Constructing a yield curve",

  "build_discount_curve_GQL",
  "Build a DiscountCurve from zero-rate nodes",
  "Constructing a yield curve; forward-curve comparison",

  "build_bond_discount_curve_GQL",
  "Bootstrap a curve from deposits and fixed-rate bonds",
  "Constructing a yield curve; day-count-convention risk",

  "build_swap_curve_GQL",
  "Bootstrap a curve from deposits and swaps",
  "EONIA/Euribor curve bootstrapping",

  "yield_curve_handle_GQL",
  "Wrap a term structure in YieldTermStructureHandle",
  "Bootstrap and index construction",

  "make_euribor6m_GQL",
  "Construct a Euribor 6M index",
  "Euribor curve bootstrapping",

  "curve_tbl_GQH",
  "Sample discount factors and zero rates at requested tenors",
  "All recipes",

  "curve_grid_tbl_GQL",
  "Expand a term structure to a dense grid",
  "Curve construction; forward-rate diagnostics",

  "curve_discount_safe_GQL",
  "Read a discount factor safely",
  "Interpolation comparison",

  "zero_spreaded_term_structure_GQL",
  "Apply a zero-rate spread to an existing curve",
  "Interest-rate sensitivities via zero spread",

  "day_counter_GQL",
  "Construct supported QuantLib day counters",
  "Dangerous day-count conventions"
)

tbl_existing_wrappers

# =============================================================================
# 1. EONIA curve bootstrapping
# =============================================================================
#
# LagrangeFinance currently covers this recipe at a higher abstraction level
# through build_swap_curve_GQL(). It does not expose the Cookbook's detailed
# EONIA helper-by-helper construction in this chapter.
#
# Existing high-level implementation:

GaussQuant::set_eval_date_GQL(
  "2008-09-15"
)

obj_curve_swap <- GaussQuant::build_swap_curve_GQL(
  settlement_date = "2008-09-18",
  fixing_days = 3
)

obj_handle_swap_curve <- GaussQuant::yield_curve_handle_GQL(
  obj_curve_swap
)

tbl_curve_swap <- GaussQuant::curve_tbl_GQH(
  curve = obj_curve_swap,
  tenors = c(
    "1W", "1M", "3M", "6M",
    "1Y", "2Y", "3Y", "5Y",
    "7Y", "10Y", "15Y"
  )
)

tbl_curve_swap

tbl_audit_eonia <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,
  ~str_comment,

  "Evaluation date",
  "covered",
  "set_eval_date_GQL",
  "Already wrapped",

  "Curve construction",
  "covered_at_high_level",
  "build_swap_curve_GQL",
  "Helper-by-helper EONIA construction is hidden",

  "Curve handle",
  "covered",
  "yield_curve_handle_GQL",
  "Already wrapped",

  "Curve inspection",
  "covered",
  "curve_tbl_GQH; curve_grid_tbl_GQL",
  "Already wrapped",

  "EONIA index construction",
  "needs_confirmation",
  NA_character_,
  "Check GaussQuant for an EONIA-specific exported constructor",

  "Deposit/OIS helper transparency",
  "not_exposed_by_chapter",
  NA_character_,
  "Potential audit-only gap, not necessarily a package defect"
)

tbl_audit_eonia

# =============================================================================
# 2. Euribor curve bootstrapping
# =============================================================================
#
# LagrangeFinance uses build_swap_curve_GQL() for the curve and
# make_euribor6m_GQL() for the floating index.

obj_index_euribor6m <- GaussQuant::make_euribor6m_GQL(
  obj_handle_swap_curve
)

tbl_audit_euribor <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,
  ~str_comment,

  "Deposit and swap curve",
  "covered",
  "build_swap_curve_GQL",
  "High-level curve builder exists",

  "Yield-curve handle",
  "covered",
  "yield_curve_handle_GQL",
  "Already wrapped",

  "Euribor 6M index",
  "covered",
  "make_euribor6m_GQL",
  "Already wrapped",

  "Market-quote helper inspection",
  "hidden",
  NA_character_,
  "Builder hides individual RateHelper objects"
)

tbl_audit_euribor

# =============================================================================
# 3. Constructing a yield curve
# =============================================================================

GaussQuant::set_eval_date_GQL(
  "2022-08-19"
)

tbl_zero_nodes <- tibble::tribble(
  ~str_tenor, ~date_node, ~num_zero_rate,
  "0D",  as.Date("2022-08-19"), 0.0005,
  "1M",  as.Date("2022-09-19"), 0.0006,
  "3M",  as.Date("2022-11-21"), 0.0008,
  "6M",  as.Date("2023-02-20"), 0.0011,
  "1Y",  as.Date("2023-08-21"), 0.0018,
  "2Y",  as.Date("2024-08-19"), 0.0032,
  "3Y",  as.Date("2025-08-19"), 0.0048,
  "5Y",  as.Date("2027-08-19"), 0.0075,
  "10Y", as.Date("2032-08-19"), 0.0115
)

obj_curve_zero <- GaussQuant::build_zero_curve_GQL(
  date_chr = as.character(tbl_zero_nodes$date_node),
  zero_rates = tbl_zero_nodes$num_zero_rate,
  day_counter = "Actual365Fixed"
)

obj_curve_discount <- GaussQuant::build_discount_curve_GQL(
  nodes = tbl_zero_nodes |>
    dplyr::transmute(
      date = date_node,
      zero_rate = num_zero_rate
    ),
  day_counter = "Actual365Fixed"
)

tbl_curve_zero <- GaussQuant::curve_tbl_GQH(
  curve = obj_curve_zero,
  tenors = c(
    "1D", "1W", "1M", "3M", "6M",
    "1Y", "2Y", "3Y", "5Y", "7Y", "10Y"
  )
)

tbl_curve_discount <- GaussQuant::curve_tbl_GQH(
  curve = obj_curve_discount,
  tenors = c(
    "1D", "1W", "1M", "3M", "6M",
    "1Y", "2Y", "3Y", "5Y", "7Y", "10Y"
  )
)

tbl_curve_zero
tbl_curve_discount

tbl_audit_curve_construction <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,

  "ZeroCurve construction",
  "covered",
  "build_zero_curve_GQL",

  "DiscountCurve construction",
  "covered",
  "build_discount_curve_GQL",

  "Deposit + bond bootstrap",
  "covered",
  "build_bond_discount_curve_GQL",

  "Deposit + swap bootstrap",
  "covered",
  "build_swap_curve_GQL",

  "Curve sampling by tenor",
  "covered",
  "curve_tbl_GQH",

  "Dense curve grid",
  "covered",
  "curve_grid_tbl_GQL"
)

tbl_audit_curve_construction

# =============================================================================
# 4. Dangerous day-count conventions
# =============================================================================
#
# LagrangeFinance explicitly distinguishes:
# - product-specific coupon day count
# - term-structure time-axis day count
#
# The Cookbook risk is therefore conceptually covered, but the chapter does not
# yet present a compact side-by-side failure demonstration.

obj_day_counter_actual_365_fixed <- GaussQuant::day_counter_GQL(
  "Actual365Fixed"
)

obj_day_counter_actual_360 <- GaussQuant::day_counter_GQL(
  "Actual360"
)

obj_day_counter_actual_actual_bond <- GaussQuant::day_counter_GQL(
  "ActualActual_Bond"
)

tbl_audit_day_count <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,
  ~str_comment,

  "Actual/365 Fixed",
  "covered",
  "day_counter_GQL",
  "Suitable for many term-structure time axes",

  "Actual/360",
  "covered",
  "day_counter_GQL",
  "Common money-market convention",

  "Actual/Actual Bond",
  "covered",
  "day_counter_GQL",
  "Requires coupon reference periods",

  "Danger demonstration",
  "partial",
  NA_character_,
  "Add a focused comparison only if Cookbook reproduction requires it"
)

tbl_audit_day_count

# =============================================================================
# 5. Implied term structures
# =============================================================================
#
# Current LagrangeFinance code checks the installed QuantLib binding directly.
# This indicates that the wrapper status must be confirmed in GaussQuant.

obj_date_implied_reference <- GaussQuant::date_GQL(
  "2009-09-18"
)

fn_implied_term_structure <- get0(
  "ImpliedTermStructure",
  envir = asNamespace("QuantLib"),
  inherits = FALSE
)

obj_curve_implied <- if (is.null(fn_implied_term_structure)) {
  NULL
} else {
  tryCatch(
    fn_implied_term_structure(
      obj_handle_swap_curve,
      obj_date_implied_reference
    ),
    error = function(e) NULL
  )
}

tbl_curve_implied <- if (is.null(obj_curve_implied)) {
  tibble::tibble(
    str_status = "ImpliedTermStructure unavailable in installed QuantLib binding"
  )
} else {
  GaussQuant::curve_tbl_GQH(
    curve = obj_curve_implied,
    tenors = c(
      "1M", "3M", "6M", "1Y",
      "2Y", "3Y", "5Y", "10Y"
    )
  )
}

tbl_curve_implied

tbl_audit_implied <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,
  ~str_comment,

  "Implied reference date",
  "covered",
  "date_GQL",
  "Already wrapped",

  "ImpliedTermStructure constructor",
  "binding_dependent",
  NA_character_,
  "LagrangeFinance currently calls QuantLib dynamically",

  "Curve inspection",
  "covered",
  "curve_tbl_GQH",
  "Already wrapped"
)

tbl_audit_implied

# =============================================================================
# 6. Interest-rate sensitivities via zero spread
# =============================================================================

num_zero_spread <- 0.005

obj_curve_spreaded <- GaussQuant::zero_spreaded_term_structure_GQL(
  curve = obj_curve_swap,
  spread = num_zero_spread
)

tbl_curve_spreaded <- GaussQuant::curve_tbl_GQH(
  curve = obj_curve_spreaded,
  tenors = c(
    "1M", "3M", "6M", "1Y",
    "2Y", "3Y", "5Y", "10Y"
  )
)

tbl_curve_spreaded

tbl_audit_zero_spread <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,

  "Parallel zero-rate shift",
  "covered",
  "zero_spreaded_term_structure_GQL",

  "Shifted-curve inspection",
  "covered",
  "curve_tbl_GQH",

  "Dense shifted-curve grid",
  "covered",
  "curve_grid_tbl_GQL"
)

tbl_audit_zero_spread

# =============================================================================
# 7. A glitch in forward-rate curves
# =============================================================================
#
# LagrangeFinance already compares ZeroCurve and DiscountCurve and derives
# adjacent-period forward rates from discount factors.

tbl_grid_zero <- GaussQuant::curve_grid_tbl_GQL(
  curve = obj_curve_zero,
  n = 300L,
  extrapolate = TRUE
) |>
  dplyr::mutate(
    str_curve = "ZeroCurve"
  )

tbl_grid_discount <- GaussQuant::curve_grid_tbl_GQL(
  curve = obj_curve_discount,
  n = 300L,
  extrapolate = TRUE
) |>
  dplyr::mutate(
    str_curve = "DiscountCurve"
  )

tbl_forward_comparison <- dplyr::bind_rows(
  tbl_grid_zero,
  tbl_grid_discount
) |>
  dplyr::group_by(
    str_curve
  ) |>
  dplyr::arrange(
    str_curve,
    time
  ) |>
  dplyr::mutate(
    num_next_time = dplyr::lead(time),
    num_next_discount = dplyr::lead(discount_factor),
    num_forward_rate = dplyr::if_else(
      !is.na(num_next_time) &
        num_next_time > time &
        discount_factor > 0 &
        num_next_discount > 0,
      -log(
        num_next_discount /
          discount_factor
      ) / (
        num_next_time -
          time
      ),
      NA_real_
    )
  ) |>
  dplyr::ungroup()

tbl_forward_comparison

tbl_audit_forward_glitch <- tibble::tribble(
  ~str_component,
  ~str_status,
  ~str_existing_function,
  ~str_comment,

  "Dense curve grid",
  "covered",
  "curve_grid_tbl_GQL",
  "Already wrapped",

  "Safe discount retrieval",
  "covered",
  "curve_discount_safe_GQL",
  "Used by LagrangeFinance interpolation comparison",

  "Forward-rate derivation",
  "implemented_in_analysis_code",
  NA_character_,
  "Potential reusable tbl_forward_curve_GQL wrapper candidate"
)

tbl_audit_forward_glitch

# =============================================================================
# Final wrapper-gap summary
# =============================================================================

tbl_wrapper_gap_summary <- tibble::tribble(
  ~str_priority,
  ~str_candidate,
  ~str_reason,

  "high",
  "Confirm or add implied_term_structure_GQL",
  "LagrangeFinance still probes QuantLib::ImpliedTermStructure dynamically",

  "medium",
  "Confirm EONIA-specific index/helper coverage",
  "High-level swap builder exists, but Cookbook helper-level transparency is absent",

  "medium",
  "Consider tbl_forward_curve_GQL",
  "Forward-rate derivation is repeated in analysis code",

  "low",
  "Calendar and convention constructors",
  "Raw QuantLib constructors remain visible but may be acceptable",

  "none",
  "ZeroCurve, DiscountCurve, bond bootstrap, swap bootstrap",
  "High-level GaussQuant builders already exist"
)

tbl_wrapper_gap_summary
