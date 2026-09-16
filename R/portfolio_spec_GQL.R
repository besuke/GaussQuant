#' Create a Portfolio Specification
#'
#' Create a tidy portfolio specification used by GaussQuant portfolio
#' optimisation functions.
#'
#' @param assets Character vector of asset names.
#' @param weights Optional initial portfolio weights.
#'
#' @return A GaussQuant portfolio specification object.
#'
#' @export
portfolio_spec_GQL <- function(
    assets,
    weights = NULL
) {
  
  rlang::check_installed("tibble")
  
  assets <- as.character(assets)
  
  if (length(assets) == 0L) {
    rlang::abort("`assets` must contain at least one asset.")
  }
  
  if (anyDuplicated(assets)) {
    rlang::abort("`assets` must contain unique asset names.")
  }
  
  n_assets <- length(assets)
  
  if (is.null(weights)) {
    weights <- rep(1 / n_assets, n_assets)
  }
  
  if (length(weights) != n_assets) {
    rlang::abort(
      "`weights` must have the same length as `assets`."
    )
  }
  
  tbl_assets <- tibble::tibble(
    asset = assets,
    weight = as.numeric(weights)
  )
  
  structure(
    list(
      assets = tbl_assets,
      constraints = tibble::tibble(),
      objectives = tibble::tibble()
    ),
    class = c(
      "portfolio_spec_GQL",
      "list"
    )
  )
}