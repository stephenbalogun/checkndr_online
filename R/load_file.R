
load_file <- function(name, path, var_names) {
  ext <- file_ext(name)

  dt <- switch(ext,
               csv = read_ndr(
                 path,
                 type = "recency"
               ),
               xlsx = read_excel(
                 path,
                 na = c("", "NA", "NULL")
               ) |> clean_names(),
               validate("Invalid file; Please upload a .csv or .xlsx file")
  )

  dt |>
    select(any_of(var_names))

}
