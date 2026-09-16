#' Add a Portfolio Objective
#'
#' Add an objective to a GaussQuant portfolio specification.
#'
#' @param portfolio A portfolio specification created by
#'   `portfolio_spec_GQL()`.
#' @param type Objective type.
#' @param measure Optional risk or return measure.
#' @param ... Additional objective parameters.
#'
#' @return Updated portfolio specification.
#'
#' @export
portfolio_objective_GQL <- function(
    portfolio,
    type = c(
      "return",
      "risk",
      "risk_budget",
      "quadratic_utility",
      "weight_concentration",
      "turnover"
    ),
    measure = NULL,
    ...
) {
  
  validate_portfolio_GQH(portfolio)
  
  type <- rlang::arg_match(type)
  
  params <- rlang::list2(...)
  
  validate_objective_GQH(
    type = type,
    measure = measure,
    params = params
  )
  
  tbl_objective <- tibble::tibble(
    objective_id = nrow(portfolio$objectives) + 1L,
    type = type,
    measure = if (is.null(measure)) {
      NA_character_
    } else {
      as.character(measure)
    },
    enabled = TRUE,
    parameters = list(params)
  )
  
  portfolio$objectives <- dplyr::bind_rows(
    portfolio$objectives,
    tbl_objective
  )
  
  portfolio
}