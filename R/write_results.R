write_results <- function(list_data, f_name, ...) {
  nms <- names(list_data)

  wb_des <- createWorkbook()

  for (i in seq_along(nms)) {
    addWorksheet(wb_des, nms[[i]])

    writeData(wb_des, sheet = nms[[i]], x = list_data[[i]], ...)

    saveWorkbook(
      wb_des,
      f_name,
      overwrite = TRUE
    )
  }
}
