library(tidyverse)
library(QuantLib)

devtools::load_all()

# -----------------------------------------------------------------------------
# Date and calendar
# -----------------------------------------------------------------------------

obj_date <- GaussQuant::DateParser_parseISO_GQL("2015-03-31")

obj_calendar_us <- QuantLib::UnitedStates(
  QuantLib::UnitedStates_GovernmentBond_get()
)

obj_calendar_italy <- QuantLib::Italy()

obj_period <- GaussQuant::period_GQL(60L, "Days")

obj_date_raw <- obj_date + obj_period

date_us <- GaussQuant::advance_days_GQL(
  calendar_obj = obj_calendar_us,
  date_obj = obj_date,
  n_days = 60L
)

date_italy <- GaussQuant::advance_days_GQL(
  calendar_obj = obj_calendar_italy,
  date_obj = obj_date,
  n_days = 60L
)

tbl_calendar_advance <- tibble::tribble(
  ~calculation, ~result_date,
  "Add 60 calendar days", obj_date_raw |> GaussQuant::str_date_ISO_GQL(),
  "Add 60 business days in US", as.character(date_us),
  "Add 60 business days in Italy", as.character(date_italy)
)

tbl_calendar_advance

# -----------------------------------------------------------------------------
# Joint calendar
# -----------------------------------------------------------------------------

obj_calendar_joint <- QuantLib::JointCalendar(
  obj_calendar_us,
  obj_calendar_italy
)

obj_date_joint <- obj_calendar_joint$advance(
  obj_date,
  obj_period
)

int_business_days_joint <- obj_calendar_joint$businessDaysBetween(
  obj_date,
  obj_date_joint
)

tbl_calendar_joint <- tibble::tribble(
  ~calculation, ~result,
  "Add 60 business days in US-Italy",
  obj_date_joint |> GaussQuant::str_date_ISO_GQL(),
  "Business days in US-Italy",
  as.character(int_business_days_joint)
)

tbl_calendar_joint

# -----------------------------------------------------------------------------
# Regular monthly schedule
# -----------------------------------------------------------------------------

obj_date_effective <- GaussQuant::DateParser_parseISO_GQL("2015-01-01")
obj_date_termination <- GaussQuant::DateParser_parseISO_GQL("2016-01-01")
obj_tenor <- GaussQuant::period_GQL("1M")

str_business_convention <- "Following"
str_termination_business_convention <- "Following"
str_date_generation <- "Forward"
flg_end_of_month <- FALSE

obj_schedule <- QuantLib::Schedule(
  obj_date_effective,
  obj_date_termination,
  obj_tenor,
  obj_calendar_us,
  str_business_convention,
  str_termination_business_convention,
  str_date_generation,
  flg_end_of_month
)

tbl_schedule <- GaussQuant::schedule_table_GQL(obj_schedule)

tbl_schedule

# -----------------------------------------------------------------------------
# Long stub at the front
# -----------------------------------------------------------------------------

obj_date_first <- GaussQuant::DateParser_parseISO_GQL("2015-02-01")
obj_date_effective <- GaussQuant::DateParser_parseISO_GQL("2014-12-15")
obj_date_termination <- GaussQuant::DateParser_parseISO_GQL("2016-01-01")

obj_schedule_front_stub <- QuantLib::Schedule(
  obj_date_effective,
  obj_date_termination,
  obj_tenor,
  obj_calendar_us,
  str_business_convention,
  str_termination_business_convention,
  "Backward",
  flg_end_of_month,
  obj_date_first
)

tbl_schedule_front_stub <- GaussQuant::schedule_table_GQL(
  obj_schedule_front_stub
)

tbl_schedule_front_stub

# -----------------------------------------------------------------------------
# Long stub at the back
# -----------------------------------------------------------------------------

obj_date_effective <- GaussQuant::DateParser_parseISO_GQL("2015-01-01")
obj_date_penultimate <- GaussQuant::DateParser_parseISO_GQL("2015-12-01")
obj_date_termination <- GaussQuant::DateParser_parseISO_GQL("2016-01-15")
obj_date_first <- QuantLib::Date()

obj_schedule_back_stub <- QuantLib::Schedule(
  obj_date_effective,
  obj_date_termination,
  obj_tenor,
  obj_calendar_us,
  str_business_convention,
  str_termination_business_convention,
  "Forward",
  flg_end_of_month,
  obj_date_first,
  obj_date_penultimate
)

tbl_schedule_back_stub <- GaussQuant::schedule_table_GQL(
  obj_schedule_back_stub
)

tbl_schedule_back_stub

# -----------------------------------------------------------------------------
# Schedule from explicit dates
# -----------------------------------------------------------------------------

vec_schedule_dates <- c(
  "2015-01-02",
  "2015-02-02",
  "2015-03-02",
  "2015-04-01",
  "2015-05-01",
  "2015-06-01",
  "2015-07-01",
  "2015-08-03",
  "2015-09-01",
  "2015-10-01",
  "2015-11-02",
  "2015-12-01",
  "2016-01-04"
)

obj_schedule_dates <- GaussQuant::make_date_vector_GQL(
  vec_schedule_dates
)

obj_schedule_explicit <- QuantLib::Schedule(
  obj_schedule_dates,
  obj_calendar_us,
  "Following"
)

tbl_schedule_explicit <- GaussQuant::schedule_table_GQL(
  obj_schedule_explicit
)

tbl_schedule_explicit
