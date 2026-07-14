# display_helpers_GQH.R



#' Format a numeric value for display
#'
#' @param x Numeric vector.
#' @param digits Number of digits.
#'
#' @return Character vector.
#' @export
fmt_num_GQH <- function(x, digits = 6) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(as.numeric(x), digits = digits, format = "fg", flag = "#")
  )
}

#' Print a compact table for examples
#'
#' @param x Object coercible to tibble.
#' @param title Optional display title.
#' @param n Number of rows to print.
#'
#' @return x invisibly.
#' @export
show_tbl_GQH <- function(x, title = NULL, n = 10) {
  if (!is.null(title)) {
    message("========================================")
    message(title)
    message("========================================")
  }

  print(utils::head(tibble::as_tibble(x), n = n))
  invisible(x)
}

#' Build a curve table
#'
#' @param curve QuantLib yield term structure.
#' @param tenors Character vector of tenors.
#'
#' @return Tibble with dates, discount factors, and zero rates.
#' @export
curve_tbl_GQH <- function(
    curve,
    tenors = c("1D", "1W", "1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y")
) {
  ref_date <- tryCatch(
    curve$referenceDate(),
    error = function(e) {
      stop(
        "curve$referenceDate() failed: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  

  tibble::tibble(
    tenor = tenors,
    date = purrr::map(.data$tenor, ~ ref_date + period_GQL(.x)),
    date_r = as.Date(
      purrr::map_chr(.data$date, iso_GQL)
    ),
    discount = purrr::map_dbl(
      .data$date,
      ~ tryCatch(curve$discount(.x), error = function(e) NA_real_)
    ),
    zero_rate = purrr::map_dbl(
      .data$date,
      function(d) {
        t <- tryCatch(
          curve$timeFromReference(d),
          error = function(e) {
            dc <- QuantLib::Actual365Fixed()
            dc$yearFraction(ref_date, d)
          }
        )

        df <- tryCatch(curve$discount(d), error = function(e) NA_real_)

        if (is.na(t) || t <= 0 || is.na(df) || df <= 0) {
          return(NA_real_)
        }

        -log(df) / t
      }
    )
  ) |>
    dplyr::select(.data$tenor, date = .data$date_r, .data$discount, .data$zero_rate)
}

#' Alias for curve_tbl_GQH
#'
#' @export
curve_table_GQH <- function(curve, tenors = c("1D", "1W", "1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y")) {
  curve_tbl_GQH(curve = curve, tenors = tenors)
}

#' Extract a value from a path-like object
#'
#' @param path Path object, matrix, data frame, or numeric vector.
#' @param i Row/time index.
#' @param j Column/path index.
#'
#' @return Numeric value, or NA_real_.
#' @export
path_value_at_GQH <- function(path, i, j = 1) {
  tryCatch(
    {
      if (is.matrix(path) || is.data.frame(path)) {
        return(as.numeric(path[i, j]))
      }

      if (is.numeric(path)) {
        return(as.numeric(path[i]))
      }

      if (!is.null(path$value)) {
        return(as.numeric(path$value[i]))
      }

      NA_real_
    },
    error = function(e) NA_real_
  )
}

#' Convert path-like simulation output to a tibble
#'
#' @param path Path object, matrix, data frame, or numeric vector.
#'
#' @return Tibble.
#' @export
path_tbl_GQH <- function(path) {
  if (is.data.frame(path)) {
    return(tibble::as_tibble(path))
  }

  if (is.matrix(path)) {
    out <- tibble::as_tibble(path)
    names(out) <- paste0("path_", seq_along(out))
    out <- dplyr::mutate(out, step = dplyr::row_number(), .before = 1)
    return(out)
  }

  if (is.numeric(path)) {
    return(tibble::tibble(step = seq_along(path), value = as.numeric(path)))
  }

  if (!is.null(path$value)) {
    return(tibble::tibble(step = seq_along(path$value), value = as.numeric(path$value)))
  }

  tibble::tibble()
}
