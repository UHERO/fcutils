#' Save a ts-boxable object in tsd format
#'
#' Writes the TSD text format read by AREMOS and UHERO's visualization. Each
#' series takes an 80-column header line (its name, optionally followed by a
#' description), a line with the date written, the first and last period and
#' the frequency, and then its values, five to a line.
#'
#' @param x a ts-boxable object (monthly, quarterly or annual). Other
#'   frequencies are written as annual without warning. Leading and trailing
#'   NAs of each series are dropped; NAs inside a series are written as
#'   1.000000E+0015, the TSD missing value.
#' @param file character string denoting the location and name of the output file
#' @param descriptions optional named character vector, `c(id = "label")`, with
#'   a description for some or all series. With descriptions, the header line
#'   holds the name in columns 1-16 and the description in columns 17-80;
#'   longer names or descriptions are cut, with a warning. Without them, the
#'   name fills the header line.
#'
#' @return the text written to `file`, invisibly
#' @export
#'
#' @examplesIf interactive()
#' quarterly_data_example |> write_tsd(file.path(tempdir(), "out.tsd"))
#' quarterly_data_example |>
#'   write_tsd(file.path(tempdir(), "out.tsd"), descriptions = c(E_NF_HI = "Nonfarm payroll jobs"))
write_tsd <- function(x, file, descriptions = NULL) {
  # drop leading and trailing NAs; NAs inside a series stay and are written
  # as the TSD missing value, so the values keep their dates
  in_long <- conv_long(x, ser_info = TRUE) %>%
    tsbox::ts_na_omit() %>%
    tsbox::ts_regular()
  in_summary <- tsbox::ts_summary(in_long)
  today <- Sys.Date()

  # fit text into a fixed-width field: cut (with a warning) or pad with spaces
  fit <- function(text, width, what, id) {
    if (nchar(text) > width) {
      warning(
        "write_tsd: ", what, " of '", id, "' exceeds ", width,
        " characters, truncated"
      )
    }
    stringr::str_pad(
      stringr::str_trunc(text, width, ellipsis = ""),
      width,
      side = "right"
    )
  }

  # header line: the name, or the name (1-16) and description (17-80)
  header_line <- function(id) {
    if (is.null(descriptions)) {
      return(stringr::str_pad(id, 80, side = "right"))
    }
    desc <- unname(descriptions[id])
    desc <- if (is.na(desc)) "" else desc
    stringr::str_c(
      fit(id, 16, "name", id),
      fit(desc, 64, "description", id)
    )
  }

  # a period as year, two-digit month/quarter/01, and "00": 2024Q3 -> "20240300"
  period_code <- function(date, freq) {
    period <- if (freq == 12) {
      lubridate::month(date)
    } else if (freq == 4) {
      lubridate::quarter(date)
    } else {
      1
    }
    stringr::str_c(lubridate::year(date), formatC(period, width = 2, flag = "0"), "00")
  }

  # second line: date written, first and last period, frequency
  info_line <- function(start, end, freq) {
    stringr::str_c(
      strrep(" ", 32),
      formatC(lubridate::month(today), width = 2), "/",
      formatC(lubridate::day(today), width = 2), "/",
      stringr::str_sub(lubridate::year(today), 3, 4),
      "0800",
      period_code(start, freq),
      period_code(end, freq),
      if (freq == 12) "M" else if (freq == 4) "Q" else "A",
      "  0",
      strrep(" ", 16)
    )
  }

  # values in E notation, 15 characters each, five to an 80-column line
  value_lines <- function(id) {
    values <- in_long$value[in_long$id == id]
    values[is.na(values)] <- 1.000000E+0015
    text <- formatC(values, digits = 6, width = 13, format = "E", flag = " ") %>%
      stringr::str_replace("E\\+", "E\\+00") %>%
      stringr::str_replace("E\\-", "E\\-00") %>%
      stringr::str_c(collapse = "")
    starts <- seq(1, nchar(text), by = 75)
    stringr::str_sub(text, starts, pmin(starts + 74, nchar(text))) %>%
      stringr::str_pad(80, side = "right")
  }

  purrr::pmap_chr(in_summary, function(id, start, end, freq, ...) {
    stringr::str_c(
      c(header_line(id), info_line(start, end, freq), value_lines(id)),
      "\r\n",
      collapse = ""
    )
  }) %>%
    stringr::str_c(collapse = "") %>%
    readr::write_file(file)
}


#' Copy time series to the clipboard as a tab-separated table (macOS only)
#'
#' @param x a ts-boxable object; it is copied in wide form, one column per
#'   series
#' @param dec number of decimals to round to (default: 2)
#'
#' @return the copied table invisibly, a tibble of text with numbers rounded
#'   to `dec` decimals
#' @export
#'
#' @examplesIf interactive()
#' monthly_data_example |> copy_tbl()
#' monthly_data_example |> copy_tbl(1)
copy_tbl <- function(x, dec = 2) {
  x %>%
    conv_long() %>%
    tsbox::ts_wide() %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ round(.x, digits = dec) %>% format(nsmall = dec)
    )) %>%
    readr::write_delim(pipe("pbcopy"), delim = "\t")
}
