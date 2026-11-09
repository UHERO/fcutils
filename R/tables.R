# **************************
# utility functions for model development ----
# **************************

#' Generate a table with time series
#'
#' @param x a ts-boxable object
#' @param tbl_start start period for table (default: 10 years before today)
#' @param tbl_end end period for table (default: 2 years after today)
#' @param percent the growth rate shown with each series: "pc" (default,
#'   period on period), "pcy" (year on year), "pca" (annualized) or "none".
#'   Any other value shows levels only. Growth is computed before the table is
#'   cut, so the first period shown has a growth rate.
#' @param time_across should time be in column headers and variable names in
#'   first column (default TRUE)
#' @param tbl_height the height of the html table in px (default 800)
#' @param save_loc file path for saving table incl. extension ("html" or "csv")
#'   (default NULL, no file). Other extensions write nothing.
#'
#' @return a tibble of the table, values rounded to 2 decimals and series
#'   sorted by name. It is returned also when a file is saved.
#' @export
#'
#' @examples
#' quarterly_data_example %>%
#'   tsbox::ts_long() %>%
#'   tsbox::ts_tslist() %>%
#'   gen_table()
#' gen_table(quarterly_data_example)
#' gen_table(quarterly_data_example, percent = "none")
#' gen_table(quarterly_data_example, percent = "pcy", time_across = FALSE)
#' @examplesIf interactive()
#' gen_table(quarterly_data_example,
#'   percent = "pcy",
#'   time_across = FALSE, save_loc = file.path(tempdir(), "temp.csv")
#' )
#' gen_table(quarterly_data_example,
#'   percent = "pcy", time_across = TRUE,
#'   save_loc = file.path(tempdir(), "temp.html")
#' )
gen_table <- function(
  x,
  tbl_start = as.character(Sys.Date() - lubridate::years(10)),
  tbl_end = as.character(Sys.Date() + lubridate::years(2)),
  percent = "pc",
  time_across = TRUE,
  tbl_height = 800,
  save_loc = NULL
) {
  tbl_start <- to_ymd(tbl_start)
  tbl_end <- to_ymd(tbl_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # add growth rates and format table for output
  tbl_out <- x_mod %>%
    {
      if (percent == "pc") {
        tsbox::ts_c(
          .,
          tsbox::ts_pc(.) %>%
            dplyr::mutate(id = stringr::str_c(.data$id, " (%)"))
        )
      } else if (percent == "pcy") {
        tsbox::ts_c(
          .,
          tsbox::ts_pcy(.) %>%
            dplyr::mutate(id = stringr::str_c(.data$id, " (YoY%)"))
        )
      } else if (percent == "pca") {
        tsbox::ts_c(
          .,
          tsbox::ts_pca(.) %>%
            dplyr::mutate(id = stringr::str_c(.data$id, " (Ann%)"))
        )
      } else if (percent == "none") {
        .
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = round(.data$value, 2)) %>%
    dplyr::arrange(.data$id) %>%
    tsbox::ts_span(tbl_start, tbl_end) %>%
    {
      if (time_across) {
        tidyr::pivot_wider(., names_from = "time", values_from = "value")
      } else {
        tidyr::pivot_wider(., names_from = "id", values_from = "value")
      }
    }

  # is a file requested?
  if (is.null(save_loc)) {
    return(tbl_out)
  }

  # file extension
  ext <- tolower(tools::file_ext(save_loc))

  # html table
  if (ext == "html") {
    if (time_across) {
      tbl_out %>%
        reactable::reactable(
          searchable = TRUE,
          # Search by case-sensitive text match

          searchMethod = htmlwidgets::JS(
            "function(rows, columnIds, searchValue) {
        const pattern = new RegExp(searchValue, 'i')
        return rows.filter(function(row) {
        return columnIds.some(function(columnId) {
        return pattern.test(row.values[columnId])
        })
        })
        }"
          ),
          columns = list(
            id = reactable::colDef(
              sticky = "left"
            )
          ),
          style = list(
            fontFamily = "monaco, sans-serif, monospace",
            fontSize = "small"
          ),
          striped = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          height = tbl_height,
          defaultPageSize = 1000,
          defaultColDef = reactable::colDef(
            format = reactable::colFormat(separators = TRUE, digits = 2)
          )
        ) %>%
        htmlwidgets::saveWidget(file = save_loc)
    } else {
      tbl_out %>%
        reactable::reactable(
          columns = list(
            time = reactable::colDef(
              sticky = "left"
            )
          ),
          style = list(
            fontFamily = "monaco, sans-serif, monospace",
            fontSize = "small"
          ),
          striped = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          height = tbl_height,
          defaultPageSize = 1000,
          defaultColDef = reactable::colDef(
            format = reactable::colFormat(separators = TRUE, digits = 2)
          )
        ) %>%
        htmlwidgets::saveWidget(file = save_loc)
    }
  }

  # csv table
  if (ext == "csv") {
    tbl_out %>% readr::write_csv(file = save_loc)
  }

  return(tbl_out)
}
