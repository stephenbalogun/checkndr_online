
invalid_cases <- function(df) {
  df |>
    filter(
      is.na(client_state) |
        is.na(client_lga) |
        is.na(sex) | !sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f") |
        is.na(age) | age < 15 |
        is.na(visit_date) |
        !hts_result %in% c("R", "Pos") |
        !hts_confirmatory_result %in% c("R", "Pos", "NR", "Neg", "Invalid") |
        hts_confirmatory_result %in% c("NR", "Neg") & !hts_tie_breaker_result %in% c("R", "Pos") |
        is.na(testing_point) |
        recency_test_name %in% c("Asante", "AS") & is.na(recency_test_date) |
        recency_test_name %in% c("Asante", "AS") & is.na(recency_number) |
        recency_test_name %in% c("Asante", "AS") & !control_line %in% c("Yes", "No") |
        recency_test_name %in% c("Asante", "AS") & !verification_line %in% c("Yes", "No") |
        recency_test_name %in% c("Asante", "AS") & !longterm_line %in% c("Yes", "No") |
        control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "Yes" & !recency_interpretation %in% "LongTerm" |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & !recency_interpretation %in% "Recent" |
        control_line %in% "Yes" & !verification_line %in% "Yes" & !longterm_line %in% "Yes" &
        !recency_interpretation %in% "Negative" |
        recency_test_name %in% c("Asante", "AS") & !control_line %in% "Yes" & !recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" & !verification_line %in% "Yes" & longterm_line %in% "Yes" & !recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & !viral_load_requested %in% "Yes" |
        control_line %in% "Yes" & verification_line %in% "yes" & !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" & is.na(date_sample_collected) |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" & visit_date < max(visit_date, na.rm = TRUE) - days(28) & is.na(date_of_viral_load_result)
    )
}
