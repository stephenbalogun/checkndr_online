
opts_one <- function() {
  c(
    "Client state", "Client LGA", "Sex", "Age", "Visit date", "Screening result", "Confirmatory test",
    "Tie breaker", "HTS result interpretation", "Testing point"
  )
}

opts_two <- function() {
  c(
    "Opt out", "Recency test name", "Recency test date", "Recency test before visit date",
    "Recency number", "Invalid recency number", "Control line", "Verification line", "Longterm line",
    "Interpreted longterm", "Interpreted recent", "Interpreted negative", "Interpreted invalid",
    "Viral load requested", "VL sample collection date", "Viral load result", "Partial duplicates"
  )
}


recency_download_opts <- function() {
  c(opts_one(), opts_two())
}
