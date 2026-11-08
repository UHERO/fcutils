# **************************
# utility functions for model development ----
# **************************

#' Generate a table with time series
#'
#' @param x a ts-boxable object
#' @param tbl_start start period for table
#' @param tbl_end end period for table
#' @param percent what type of percent should be added ("none", "pc" (default),
#'   "pcy", "pca")
#' @param time_across should time be in column headers and variable names in
#'   first column (default TRUE)
#' @param tbl_height the height of the table in px (default 800)
#' @param save_loc file path for saving table incl. extension ("html" or "csv")
#'   (default NULL)
#'
#' @return table formatted for output
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
#'   time_across = FALSE, save_loc = "~/Downloads/temp.csv"
#' )
#' gen_table(quarterly_data_example,
#'   percent = "pcy", time_across = TRUE,
#'   save_loc = "~/Downloads/temp.html"
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
