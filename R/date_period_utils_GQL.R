# Date and period utilities ------------------------------------------------

.as_ql_date_GQL <- function(x) {
  if (inherits(x, "_p_Date")) {
    return(x)
  }

  date_GQL(x)
}

#' Convert QuantLib object to character
#'
#' @param x Object to print.
#'
#' @return Character scalar.
#' @export
chr_GQL <- function(x) {
  tryCatch(
    x$`__str__`(),
    error = function(e1) {
      tryCatch(
        as.character(x),
        error = function(e2) "<unprintable>"
      )
    }
  )
}

#' Set QuantLib evaluation date
#'
#' @param eval_date ISO date string, R Date, or QuantLib Date.
#'
#' @return The QuantLib Date invisibly.
#' @export
set_eval_date_GQL <- function(eval_date) {
  d <- .as_ql_date_GQL(eval_date)
  QuantLib::Settings_instance()$setEvaluationDate(d)
  invisible(d)
}

#' Advance a QuantLib date by calendar days
#'
#' @param calendar_obj QuantLib calendar object.
#' @param date_obj ISO date string, R Date, or QuantLib Date.
#' @param n_days Number of days.
#'
#' @return A QuantLib Date object.
#' @export
advance_days_GQL <- function(calendar_obj, date_obj, n_days) {
  QuantLib::Calendar_advance(
    calendar_obj,
    .as_ql_date_GQL(date_obj),
    as.integer(n_days),
    "Days"
  )
}

#' Build a QuantLib Period
#'
#' @param x Tenor string such as 1D, 1W, 3M, 18M, or 2Y.
#' @param unit Optional unit when x is numeric.
#'
#' @return A QuantLib Period object.
#' @export
period_GQL <- function(x = 1, unit = NULL) {
  if (!is.null(unit)) {
    unit <- tolower(as.character(unit)[1])
    n <- as.integer(x)

    if (unit %in% c("d", "day", "days")) {
      return(period_days_GQL(n))
    }

    if (unit %in% c("w", "week", "weeks")) {
      return(period_weeks_GQL(n))
    }

    if (unit %in% c("m", "month", "months")) {
      return(period_months_GQL(n))
    }

    if (unit %in% c("y", "year", "years")) {
      return(period_years_GQL(n))
    }

    stop("Unsupported period unit: ", unit, call. = FALSE)
  }

  if (length(x) != 1 || is.na(x)) {
    stop("tenor must be a single non-NA string", call. = FALSE)
  }

  x <- trimws(toupper(as.character(x)))

  m <- regexec("^([0-9]+)\\s*([DWMY])$", x)
  hit <- regmatches(x, m)[[1]]

  if (length(hit) == 0) {
    stop(
      "Unsupported tenor format: ", x,
      ". Use forms like 1D, 1W, 3M, 18M, 2Y.",
      call. = FALSE
    )
  }

  n <- as.integer(hit[2])
  u <- hit[3]

  switch(
    u,
    "D" = period_days_GQL(n),
    "W" = period_weeks_GQL(n),
    "M" = period_months_GQL(n),
    "Y" = period_years_GQL(n),
    stop("Unsupported tenor unit: ", u, call. = FALSE)
  )
}

#' Build a QuantLib day Period
#'
#' @param n Number of days.
#'
#' @return A QuantLib Period object.
#' @export
period_days_GQL <- function(n) {
  QuantLib::Period(as.integer(n), "Days")
}

#' Build a QuantLib week Period
#'
#' @param n Number of weeks.
#'
#' @return A QuantLib Period object.
#' @export
period_weeks_GQL <- function(n) {
  QuantLib::Period(as.integer(n), "Weeks")
}

#' Build a QuantLib month Period
#'
#' @param n Number of months.
#'
#' @return A QuantLib Period object.
#' @export
period_months_GQL <- function(n) {
  QuantLib::Period(as.integer(n), "Months")
}

#' Build a QuantLib year Period
#'
#' @param n Number of years.
#'
#' @return A QuantLib Period object.
#' @export
period_years_GQL <- function(n) {
  QuantLib::Period(as.integer(n), "Years")
}

#' Build and print a QuantLib Period
#'
#' @param x Tenor string such as 1D, 1W, 3M, or 2Y.
#'
#' @return Character scalar.
#' @export
period_chr_GQL <- function(x) {
  chr_GQL(period_GQL(x))
}

#' Extract a date from a QuantLib schedule
#'
#' @param schedule QuantLib Schedule object.
#' @param i_one_based One-based date index.
#'
#' @return A QuantLib Date object, or NULL if extraction fails.
#' @export
schedule_date_at_GQL <- function(schedule, i_one_based) {
  idx0 <- as.integer(i_one_based - 1L)

  out <- tryCatch(
    schedule$date(idx0),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  out <- tryCatch(
    schedule$dates()[[as.integer(i_one_based)]],
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  tryCatch(
    schedule[[as.integer(i_one_based)]],
    error = function(e) NULL
  )
}

#' Extract a schedule date as local R Date
#'
#' @param schedule QuantLib Schedule object.
#' @param i_one_based One-based date index.
#'
#' @return R Date, or NA if extraction fails.
#' @export
schedule_date_at_local_GQL <- function(schedule, i_one_based) {
  d <- schedule_date_at_GQL(schedule, i_one_based)

  if (is.null(d)) {
    return(as.Date(NA))
  }

  as.Date(iso_GQL(d))
}

#' Build a table of QuantLib schedule dates
#'
#' @param schedule QuantLib Schedule object.
#'
#' @return A tibble with schedule_date.
#' @export
schedule_dates_GQL <- function(schedule) {
  n <- tryCatch(
    schedule$size(),
    error = function(e) NA_integer_
  )

  if (is.na(n) || n <= 0) {
    return(tibble::tibble(schedule_date = character()))
  }

  tibble::tibble(
    schedule_date = purrr::map_chr(
      seq_len(n),
      function(i) iso_GQL(schedule_date_at_GQL(schedule, i))
    )
  )
}
