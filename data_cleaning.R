
# LOAD LIBRARIES ----------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  janitor
)
# LOAD DATA ---------------------------------------------------------------
census_tracker_file <- readxl::read_xlsx("data/UNFPA Global  Census Tracker.xlsx", 
                                         sheet = "2020 Round",
                                         skip = 1
                                         ) |> janitor::clean_names()

# TRANSFORM DATA ----------------------------------------------------------

census_data <- census_tracker_file |>
  dplyr::filter(country != "NA") |>
  dplyr::mutate(
    country = stringr::str_to_upper(country),
    unfpa_region = dplyr::case_when(is.na(unfpa_region) ~ "Non-UNFPA", T ~ unfpa_region) |>
                   stringr::str_to_upper(),
    originally_planned_census_month = tidyr::replace_na(originally_planned_census_month, "January") |>
                                      stringr::str_to_title(),
    enumeration_completed_y_n_planned = stringr::str_to_title(enumeration_completed_y_n_planned),
    at_least_1_census_in_2020_round_y_n_planned = stringr::str_to_title(at_least_1_census_in_2020_round_y_n_planned),
    actual_enumeration_date_month = stringr::str_to_title(actual_enumeration_date_month) |> 
                                    tidyr::replace_na("January"),
    actual_enumeration_date_month = stringr::word(actual_enumeration_date_month, 1, sep = "-"),
    actual_enumeration_date_month = stringr::word(actual_enumeration_date_month, 1, sep = ","),
    actual_enumeration_date_month = stringr::word(actual_enumeration_date_month, 1, sep = " "),
    actual_enumeration_date_month = dplyr::case_when(
                                      actual_enumeration_date_month == "Sept" ~ "September",
                                      actual_enumeration_date_month == "44334.0" ~ "January",
                                      actual_enumeration_date_month == "13" ~ "October",
                                      actual_enumeration_date_month == "Mar" ~ "March",
                                      actual_enumeration_date_month == "Nov" ~ "November",
                                      actual_enumeration_date_month == "Jan" ~ "January",
                                      actual_enumeration_date_month == "Buld:jan" ~ "January",
                                      actual_enumeration_date_month == "No" ~ "November",
                                      actual_enumeration_date_month == "14" ~ "November",
                                      actual_enumeration_date_month == "45901.0" ~ "January",
                                      actual_enumeration_date_month == "5" ~ "April",
                                      actual_enumeration_date_month == "25" ~ "December",
                                      actual_enumeration_date_month == "Sep" ~ "September",
                                      actual_enumeration_date_month == "45412.0" ~ "January",
                                      actual_enumeration_date_month == "Extended" ~ "January",
                                      actual_enumeration_date_month == "Oct" ~ "October",
                                      actual_enumeration_date_month == "Na" ~ "January",
                                      actual_enumeration_date_month == "Q4" ~ "October",
                                      actual_enumeration_date_month == "1" ~ "October",
                                      T ~ actual_enumeration_date_month
                                    ),
    enumeration_category = dplyr::case_when(
                            actual_enumeration_date_year > 2024 ~ "No Census Conducted",
                            is.na(actual_enumeration_date_year) ~ "No Census Conducted",
                            T ~ as.character(actual_enumeration_date_year)
                          )
  )



# DATA EXPORT -------------------------------------------------------------

write_rds(x = census_data, file = "data/clean_census_data.rds")





