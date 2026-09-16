# Internal validation helpers for GaussQuant portfolio specifications.


validate_portfolio_GQH <- function(portfolio) {
  
  if (!inherits(portfolio, "portfolio_spec_GQL")) {
    rlang::abort(
      "`portfolio` must be created by `portfolio_spec_GQL()`."
    )
  }
  
  invisible(TRUE)
}


validate_constraint_GQH <- function(
    portfolio,
    type,
    params
) {
  
  validate_portfolio_GQH(portfolio)
  
  n_assets <- nrow(portfolio$assets)
  
  switch(
    type,
    
    weight_sum = {
      
      required <- c("min_sum", "max_sum")
      
      missing <- setdiff(required, names(params))
      
      if (length(missing) > 0L) {
        rlang::abort(
          paste0(
            "`weight_sum` requires: ",
            paste(required, collapse = ", "),
            "."
          )
        )
      }
      
      if (!is.numeric(params$min_sum) ||
          length(params$min_sum) != 1L ||
          !is.numeric(params$max_sum) ||
          length(params$max_sum) != 1L) {
        rlang::abort(
          "`min_sum` and `max_sum` must be numeric scalars."
        )
      }
      
      if (params$min_sum > params$max_sum) {
        rlang::abort(
          "`min_sum` must be less than or equal to `max_sum`."
        )
      }
    },
    
    box = {
      
      required <- c("min", "max")
      
      missing <- setdiff(required, names(params))
      
      if (length(missing) > 0L) {
        rlang::abort(
          "`box` requires `min` and `max`."
        )
      }
      
      valid_length <- function(x) {
        is.numeric(x) &&
          length(x) %in% c(1L, n_assets)
      }
      
      if (!valid_length(params$min) ||
          !valid_length(params$max)) {
        rlang::abort(
          paste0(
            "`min` and `max` must be numeric with length 1 ",
            "or the number of assets."
          )
        )
      }
      
      min_weight <- rep(params$min, length.out = n_assets)
      max_weight <- rep(params$max, length.out = n_assets)
      
      if (any(min_weight > max_weight)) {
        rlang::abort(
          "Each `min` weight must be less than or equal to `max`."
        )
      }
    },
    
    group = {
      
      required <- c(
        "groups",
        "group_min",
        "group_max"
      )
      
      missing <- setdiff(required, names(params))
      
      if (length(missing) > 0L) {
        rlang::abort(
          paste0(
            "`group` requires: ",
            paste(required, collapse = ", "),
            "."
          )
        )
      }
      
      if (length(params$groups) != n_assets) {
        rlang::abort(
          "`groups` must have one value for each asset."
        )
      }
      
      n_groups <- length(unique(params$groups))
      
      if (length(params$group_min) %in% c(1L, n_groups) == FALSE ||
          length(params$group_max) %in% c(1L, n_groups) == FALSE) {
        rlang::abort(
          paste0(
            "`group_min` and `group_max` must have length 1 ",
            "or the number of groups."
          )
        )
      }
    },
    
    factor_exposure = {
      
      required <- c(
        "factor_loadings",
        "min",
        "max"
      )
      
      missing <- setdiff(required, names(params))
      
      if (length(missing) > 0L) {
        rlang::abort(
          paste0(
            "`factor_exposure` requires: ",
            paste(required, collapse = ", "),
            "."
          )
        )
      }
      
      if (!is.matrix(params$factor_loadings) &&
          !is.data.frame(params$factor_loadings)) {
        rlang::abort(
          "`factor_loadings` must be a matrix or data frame."
        )
      }
      
      if (nrow(params$factor_loadings) != n_assets) {
        rlang::abort(
          "`factor_loadings` must contain one row per asset."
        )
      }
    },
    
    position_limit = {
      
      if (is.null(params$max_positions) &&
          is.null(params$max_long) &&
          is.null(params$max_short)) {
        rlang::abort(
          paste0(
            "`position_limit` requires at least one of ",
            "`max_positions`, `max_long`, or `max_short`."
          )
        )
      }
    },
    
    turnover = {
      
      if (is.null(params$target)) {
        rlang::abort(
          "`turnover` requires `target`."
        )
      }
      
      if (!is.numeric(params$target) ||
          length(params$target) != 1L ||
          params$target < 0) {
        rlang::abort(
          "`target` must be a non-negative numeric scalar."
        )
      }
    },
    
    transaction_cost = {
      
      if (is.null(params$cost)) {
        rlang::abort(
          "`transaction_cost` requires `cost`."
        )
      }
      
      if (!is.numeric(params$cost) ||
          any(params$cost < 0)) {
        rlang::abort(
          "`cost` must contain non-negative numeric values."
        )
      }
    },
    
    diversification = {
      
      if (is.null(params$target)) {
        rlang::abort(
          "`diversification` requires `target`."
        )
      }
      
      if (!is.numeric(params$target) ||
          length(params$target) != 1L) {
        rlang::abort(
          "`target` must be a numeric scalar."
        )
      }
    },
    
    leverage = {
      
      if (is.null(params$max_leverage)) {
        rlang::abort(
          "`leverage` requires `max_leverage`."
        )
      }
      
      if (!is.numeric(params$max_leverage) ||
          length(params$max_leverage) != 1L ||
          params$max_leverage <= 0) {
        rlang::abort(
          "`max_leverage` must be a positive numeric scalar."
        )
      }
    },
    
    return = {
      
      if (is.null(params$target)) {
        rlang::abort(
          "`return` requires `target`."
        )
      }
      
      if (!is.numeric(params$target) ||
          length(params$target) != 1L) {
        rlang::abort(
          "`target` must be a numeric scalar."
        )
      }
    },
    
    rlang::abort(
      paste0(
        "Unsupported portfolio constraint type: `",
        type,
        "`."
      )
    )
  )
  
  invisible(TRUE)
}


validate_objective_GQH <- function(
    type,
    measure,
    params
) {
  
  supported_measures <- list(
    return = c(
      "mean"
    ),
    risk = c(
      "StdDev",
      "VaR",
      "ES",
      "CVaR"
    ),
    risk_budget = c(
      "StdDev",
      "VaR",
      "ES",
      "CVaR"
    ),
    quadratic_utility = c(
      "mean_variance"
    ),
    weight_concentration = c(
      "HHI"
    ),
    turnover = c(
      "turnover"
    )
  )
  
  if (!type %in% names(supported_measures)) {
    rlang::abort(
      paste0(
        "Unsupported portfolio objective type: `",
        type,
        "`."
      )
    )
  }
  
  if (!is.null(measure) &&
      !measure %in% supported_measures[[type]]) {
    rlang::abort(
      paste0(
        "Unsupported measure `",
        measure,
        "` for objective type `",
        type,
        "`."
      )
    )
  }
  
  if (type %in% c("risk", "risk_budget") &&
      measure %in% c("VaR", "ES", "CVaR")) {
    
    confidence_level <- params$confidence_level
    
    if (!is.null(confidence_level)) {
      
      if (!is.numeric(confidence_level) ||
          length(confidence_level) != 1L ||
          confidence_level <= 0 ||
          confidence_level >= 1) {
        rlang::abort(
          "`confidence_level` must be between 0 and 1."
        )
      }
    }
  }
  
  invisible(TRUE)
}