if (!requireNamespace("pacman")) install.packages("pacman", dependencies = TRUE)

pacman::p_load(
  bslib,
  dplyr,
  DT,
  glue,
  htmltools,
  janitor,
  lubridate,
  openxlsx,
  readxl,
  scales,
  shiny,
  shinyFeedback,
  shinyWidgets,
  stringr,
  summaryBox,
  tibble,
  tidyndr,
  tidyselect,
  tools
)
