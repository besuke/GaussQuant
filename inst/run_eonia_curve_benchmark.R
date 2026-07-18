rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(QuantLib)
  library(GaussQuant)
})

benchmark <- GaussQuant::eonia_curve_benchmark_GQL()

GaussQuant::show_tbl_GQH(
  benchmark$initial_validation$quote_repricing,
  "EONIA helper repricing",
  n = 40
)

GaussQuant::show_tbl_GQH(
  benchmark$jump_summary,
  "EONIA year-end jump"
)

GaussQuant::show_tbl_GQH(
  benchmark$final_validation$quote_repricing |>
    dplyr::select(
      quote_id,
      market_quote,
      implied_quote,
      quote_error,
      pillar_date,
      discount_factor,
      zero_rate,
      one_day_forward_rate
    ),
  "EONIA discount, spot, and forward rates",
  n = 40
)

cat("\nEONIA curve bootstrap benchmark completed successfully.\n")
