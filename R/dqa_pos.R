dqa_pos <- function(df) {
  obs_client_state <- glue("{sum(is.na(df$client_state), na.rm = TRUE)} clients did not have a documented state of residence")

  obs_client_lga <- glue("{sum(is.na(df$client_lga), na.rm = TRUE)} clients did not have a documented LGA of residence")

  obs_sex <- glue("{sum(is.na(df$sex) + !df$sex %in% c('M', 'F', 'Male', 'Female', 'male', 'female'), na.rm = TRUE)} clients did not have a documented gender or is neither 'Male' nor 'Female'")

  obs_age <- glue("{sum(is.na(df$age), na.rm = TRUE) + sum(df$age < 15, na.rm = TRUE)} clients either did not have a documented age or the age documented is less than 15 years")

  obs_visit_date <- glue("{sum(is.na(df$visit_date), na.rm = TRUE)} clients did not have a documented visit date")

  obs_hts_result <- glue("{sum(!df$hts_screening_result %in% c('R', 'Pos'), na.rm = TRUE)} clients did not have a 'reactive' screening result")

  obs_hts_confirmatory <- glue("{sum(!df$hts_confirmatory_result %in% c('R', 'Pos', 'NR', 'Neg', 'Invalid'), na.rm = TRUE)} clients did not have a documented confirmatory result")

  obs_hts_tie <- glue("{sum(df$hts_confirmatory_result %in% c('NR', 'Neg') & !df$hts_tie_breaker_result %in% c('R', 'Pos'), na.rm = TRUE)} clients with negative confirmatory result did not have a positive tie breaker")

  obs_wrong_hts <- glue("{sum(df$hts_result %in% c('Pos', 'POS', 'pos') & (df$hts_screening_result %in% c('R', 'Pos') + df$hts_confirmatory_result %in% c('R', 'Pos') + df$hts_tie_breaker_result %in% c('R', 'Pos'))  < 2 | df$hts_result %in% c('Neg', 'neg', 'NEG') & (df$hts_screening_result %in% c('R', 'Pos') + df$hts_confirmatory_result %in% c('R', 'Pos') + df$hts_tie_breaker_result %in% c('R', 'Pos')) >= 2, na.rm = TRUE)} clients have their HTS test results wrongly interpreted")

  obs_testing_point <- glue("{sum(is.na(df$testing_point), na.rm = TRUE)} clients did not have a documented HTS testing point")

  tribble(
    ~variables, ~observations,
    "Client state", obs_client_state,
    "Client LGA", obs_client_lga,
    "Sex", obs_sex,
    "Age", obs_age,
    "Visit date", obs_visit_date,
    "Screening result", obs_hts_result,
    "Confirmatory test", obs_hts_confirmatory,
    "Tie breaker test", obs_hts_tie,
    "HTS result interpretation", obs_wrong_hts,
    "HTS testing point", obs_testing_point
  )
}
