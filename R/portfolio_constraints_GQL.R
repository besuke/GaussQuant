#' Add a Portfolio Constraint
#'
#' Add a constraint to a GaussQuant portfolio specification.
#'
#' @param portfolio A portfolio specification created by
#'   `portfolio_spec_GQL()`.
#' @param type Constraint type.
#' @param ... Constraint parameters.
#'
#' @return Updated portfolio specification.
#'
#' @export
portfolio_constraint_GQL <- function(
    portfolio,
    type = c(
      "weight_sum",
      "box",
      "group",
      "factor_exposure",
      "position_limit",
      "turnover",
      "transaction_cost",
      "diversification",
      "leverage",
      "return"
    ),
    ...
) {
  
  validate_portfolio_GQH(portfolio)
  
  type <- rlang::arg_match(type)
  
  params <- rlang::list2(...)
  
  validate_constraint_GQH(
    portfolio = portfolio,
    type = type,
    params = params
  )
  
  tbl_constraint <- tibble::tibble(
    constraint_id = nrow(portfolio$constraints) + 1L,
    type = type,
    enabled = TRUE,
    parameters = list(params)
  )
  
  portfolio$constraints <- dplyr::bind_rows(
    portfolio$constraints,
    tbl_constraint
  )
  
  portfolio
}