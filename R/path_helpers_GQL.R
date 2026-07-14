# path_helpers_GQL.R
#
# Internal helpers for converting QuantLib simulation paths.

path_tbl_GQL <- function(path, path_id = 1L) {
  n_points <- as.integer(path$length())

  if (n_points <= 0L) {
    return(
      tibble::tibble(
        path_id = integer(),
        step = integer(),
        time = double(),
        price = double()
      )
    )
  }

  path_index <- seq_len(n_points) - 1L

  tibble::tibble(
    path_id = rep(as.integer(path_id), n_points),
    step = path_index,
    time = purrr::map_dbl(
      path_index,
      ~ as.numeric(path$time(.x))
    ),
    price = purrr::map_dbl(
      path_index,
      ~ as.numeric(path$value(.x))
    )
  )
}
