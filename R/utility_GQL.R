# utility.R

#' Convert ISO date to QuantLib Date
#'
#' @param x Date or character scalar.
#'
#' @return QuantLib Date object.
#'
#' @export
date_GQL <- function(x) {
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  }

  stopifnot(is.character(x), length(x) == 1)

  QuantLib::DateParser_parseISO(x)
}


#' Convert QuantLib Date to ISO string
#'
#' @param x QuantLib Date object.
#'
#' @return ISO date string.
#'
#' @export
iso_GQL <- function(x) {
  tryCatch(
    QuantLib::Date_ISO(x),
    error = function(e) as.character(x)
  )
}


#' Set QuantLib evaluation date
#'
#' @param x Date or character scalar.
#'
#' @return Invisibly returns QuantLib Date object.
#'
#' @export
eval_date_GQL <- function(x) {
  d <- date_GQL(x)

  invisible(
    QuantLib::Settings_instance()$setEvaluationDate(d = d)
  )

  invisible(d)
}


#' Get QuantLib evaluation date
#'
#' @return ISO date string.
#'
#' @export
eval_date_get_GQL <- function() {
  settings <- QuantLib::Settings_instance()

  d <- tryCatch(
    QuantLib::Settings_getEvaluationDate(settings),
    error = function(e1) {
      tryCatch(
        settings$evaluationDate(),
        error = function(e2) {
          stop(
            "Failed to get QuantLib evaluation date. ",
            conditionMessage(e1),
            call. = FALSE
          )
        }
      )
    }
  )

  iso_GQL(d)
}


#' Select QuantLib day counter
#'
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return QuantLib day counter object.
#'
#' @export
day_counter_GQL <- function(day_counter = "Actual365Fixed") {
  if (!is.character(day_counter)) {
    return(day_counter)
  }

  switch(
    day_counter,
    Actual365Fixed = QuantLib::Actual365Fixed(),
    Actual360 = QuantLib::Actual360(),
    Thirty360 = QuantLib::Thirty360(),
    Thirty360_European = QuantLib::Thirty360("European"),
    ActualActual_ISDA = QuantLib::ActualActual("ISDA"),
    ActualActual_Bond = QuantLib::ActualActual("Bond"),
    stop("Unsupported day counter: ", day_counter, call. = FALSE)
  )
}


#' Build QuantLib DateVector
#'
#' @param dates Character vector or Date vector.
#'
#' @return QuantLib DateVector.
#'
#' @export
make_date_vector_GQL <- function(dates) {
  dv <- QuantLib::DateVector()

  purrr::walk(
    dates,
    function(d) {
      QuantLib::DateVector_append(dv, date_GQL(d))
    }
  )

  dv
}


#' Create QuantLib QuoteHandle
#'
#' @param x Numeric quote.
#'
#' @return QuantLib QuoteHandle.
#'
#' @export
quote_handle_GQL <- function(x) {
  QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(x)
  )
}


#' Push rate helpers into QuantLib RateHelperVector
#'
#' @param helpers List of QuantLib rate helpers.
#'
#' @return QuantLib RateHelperVector.
#'
#' @export
push_rate_helpers_GQL <- function(helpers) {
  vec <- QuantLib::RateHelperVector()

  purrr::walk(
    helpers,
    ~ QuantLib::RateHelperVector_push_back(vec, .x)
  )

  vec
}


#' Convert QuantLib leg to cashflow table
#'
#' @param leg QuantLib Leg or CashFlow vector.
#'
#' @return A tibble with cashflow dates and amounts.
#'
#' @export
leg_to_cashflow_tbl_GQL <- function(leg) {
  tibble::tibble(
    idx = seq_len(leg$size())
  ) |>
    dplyr::mutate(
      cashflow = purrr::map(
        .data$idx,
        function(i) leg[i][[1]]
      ),
      date = purrr::map_chr(
        .data$cashflow,
        function(cf) {
          iso_GQL(
            QuantLib::CashFlow_date(cf)
          )
        }
      ),
      amount = purrr::map_dbl(
        .data$cashflow,
        function(cf) {
          QuantLib::CashFlow_amount(cf)
        }
      )
    ) |>
    dplyr::select(
      .data$date,
      .data$amount
    )
}
