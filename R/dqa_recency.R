dqa_recency <- function(df) {
  obs_opt_out <- glue("{sum(is.na(df$opt_out) & !is.na(df$recency_test_name), na.rm = TRUE)} clients did not have a documented `opt_out` status but had Asante recency test done")

  obs_recency_test_name <- glue("{sum(!df$recency_test_name %in% c('Asante', 'AS'), na.rm = TRUE)} clients did not opt out of recency testing but have no recency test name}")

  obs_recency_test_date <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_test_date), na.rm = TRUE)} clients have a documented recency test but no date of testing for recency")

  obs_test_before_visit <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & df$recency_test_date < df$visit_date, na.rm = TRUE)} clients had their recency test date preceding the service visit date")

  obs_recency_number <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_number), na.rm = TRUE)} clients have a documented recency test but no recency number")

  obs_valid_recency_number <- glue("{sum(!stringr::str_detect(df$recency_number, '[:alpha:]{2}[:digit:]{8}'), na.rm = TRUE)} clients did not have the expected format for the recency number")

  obs_control_line <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the control line outcome")

  obs_verification_line <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$verification_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the verification line outcome")

  obs_longterm_line <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$longterm_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the long-term line outcome")

  obs_interpretation_longterm <- glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'LongTerm', na.rm = TRUE)} clients with all lines visible were not documented as 'Long-term' results")

  obs_interpretation_recent <- glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Recent', na.rm = TRUE)} clients with only control and verification lines visible were not documented as 'Recent' results")

  obs_interpretation_negative <- glue("{sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Negative', na.rm = TRUE)} clients with only the control line were not documented as 'Negative' results")

  obs_interpretation_invalid <- glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid', na.rm = TRUE) + sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid', na.rm = TRUE)} clients with either no control line or have longterm line but no verification line but were not documented as 'Invalid' results")

  obs_vl_request <- glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$viral_load_requested %in% 'Yes', na.rm = TRUE)} clients were identified as 'Recent' but did not have their viral load samples requested")

  obs_vl_sample <- glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & is.na(df$date_sample_collected), na.rm = TRUE)} clients were identified as 'Recent' but did not have their date of sample colllection documented")

  obs_vl_result <- glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & df$recency_test_date < max(df$recency_test_date, na.rm = TRUE) - lubridate::days(28) & is.na(df$date_of_viral_load_result), na.rm = TRUE)} clients with RTRI_RECENT results were identified over 4 weeks ago but are still without viral load result")

  obs_partial_duplicates <- glue("{nrow(df |>  janitor::get_dupes(sex, date_of_birth, facility, visit_date))} entries are at least partially duplicated with the same 'sex', 'date_of_birth', 'facility', 'visit_date' and 'HIV status'")

  tribble(
    ~variables, ~observations,
    "Opt out", obs_opt_out,
    "Recency test name", obs_recency_test_name,
    "Recency test date", obs_recency_test_date,
    "Recency date date before visit date", obs_test_before_visit,
    "Recency number", obs_recency_number,
    "Valid recency number", obs_valid_recency_number,
    "Control line", obs_control_line,
    "Verification line", obs_verification_line,
    "Longterm line", obs_longterm_line,
    "Interpreted long-term", obs_interpretation_longterm,
    "Interpreted recent", obs_interpretation_recent,
    "Interpreted negative", obs_interpretation_negative,
    "Interpreted invalid", obs_interpretation_invalid,
    "Viral load requested", obs_vl_request,
    "Viral load sample date", obs_vl_sample,
    "Viral load results", obs_vl_result,
    "Partial duplicates", obs_partial_duplicates
  )
}
