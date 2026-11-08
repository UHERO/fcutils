#' Save a ts-boxable object in tsd format
#'
#' @param x a ts-boxable object (only M, Q, A frequency)
#' @param file character string denoting the location and name of the output file
#'
#' @return nothing (silently save the contents of the tsd file to a user defined
#'   location)
#' @export
#'
#' @examplesIf interactive()
#' quarterly_data_example |> write_tsd("out.tsd")
write_tsd <- function(x, file) {
  # convert the ts-boxable object to tslist
  x_mod <- conv_long(x, ser_info = TRUE)
  # in_list <- x_mod %>%
  #   tidyr::drop_na() %>%
  #   tsbox::ts_tslist()
  in_list <- x_mod %>%
    tidyr::drop_na()

  # get summary info about the time series
  in_summary <- in_list %>%
    tsbox::ts_summary()

  # get character strings with the contents of individual series
  get_ser_string <- function(ser_i) {
    # add padding to the name of the series
    ser_name <- stringr::str_c(
      ser_i,
      stringr::str_c(rep(" ", 80 - nchar(ser_i)), collapse = ""),
      "\r\n"
    )

    # combine additional time series information into a character string
    ser_time <- stringr::str_c(
      stringr::str_c(rep(" ", 32), collapse = ""),
      # current date
      Sys.Date() %>%
        lubridate::month() %>%
        formatC(width = 2),
      "/",
      Sys.Date() %>% lubridate::day() %>% formatC(width = 2),
      "/",
      Sys.Date() %>% lubridate::year() %>% stringr::str_sub(3, 4),
      "0800",
      # starting year and period
      in_summary %>%
        dplyr::filter(.data$id == ser_i) %>%
        dplyr::pull(.data$start) %>%
        lubridate::year(),
      dplyr::case_when(
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          12 ~
          in_summary %>%
          dplyr::filter(.data$id == ser_i) %>%
          dplyr::pull(start) %>%
          lubridate::month() %>%
          formatC(width = 2, flag = "0"),
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          4 ~
          in_summary %>%
          dplyr::filter(.data$id == ser_i) %>%
          dplyr::pull(start) %>%
          lubridate::quarter() %>%
          formatC(width = 2, flag = "0"),
        TRUE ~ "01"
      ),
      "00",
      # ending year and period
      in_summary %>%
        dplyr::filter(.data$id == ser_i) %>%
        dplyr::pull(.data$end) %>%
        lubridate::year(),
      dplyr::case_when(
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          12 ~
          in_summary %>%
          dplyr::filter(.data$id == ser_i) %>%
          dplyr::pull(end) %>%
          lubridate::month() %>%
          formatC(width = 2, flag = "0"),
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          4 ~
          in_summary %>%
          dplyr::filter(.data$id == ser_i) %>%
          dplyr::pull(end) %>%
          lubridate::quarter() %>%
          formatC(width = 2, flag = "0"),
        TRUE ~ "01"
      ),
      "00",
      # frequency label
      dplyr::case_when(
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          12 ~
          "M",
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) ==
          4 ~
          "Q",
        TRUE ~ "A"
      ),
      stringr::str_c(rep(" ", 2), collapse = ""),
      "0",
      stringr::str_c(rep(" ", 16), collapse = ""),
      # "  0                ",
      "\r\n"
    )

    # replace missing values in the data
    ser_data <- in_list %>%
      # purrr::pluck(ser_i) %>%
      # tsbox::ts_tbl() %>%
      dplyr::filter(.data$id == ser_i) %>%
      dplyr::mutate(
        value = dplyr::if_else(is.na(.data$value), 1.000000E+0015, .data$value)
      ) %>%
      dplyr::pull(.data$value) %>%
      formatC(digits = 6, width = 13, format = "E", flag = " ") %>%
      stringr::str_replace("E\\+", "E\\+00") %>%
      stringr::str_replace("E\\-", "E\\-00") %>%
      stringr::str_c(collapse = "")

    # find break points in the long data string
    string_length <- ser_data %>% nchar()
    split_points <- seq(0, string_length, 75)

    # calculate the start and end of rows
    if (split_points %>% utils::tail(1) == string_length) {
      string_starts <- (split_points + 1) %>% utils::head(-1)
      string_ends <- split_points %>% utils::tail(-1)
    } else {
      string_starts <- (split_points + 1)
      string_ends <- split_points %>%
        utils::tail(-1) %>%
        c(string_length)
    }

    # select data for each row and add padding
    ser_string <- ser_data %>%
      stringr::str_sub(string_starts, string_ends) %>%
      purrr::map(
        ~ stringr::str_c(
          .x,
          stringr::str_c(rep(" ", 80 - .x %>% nchar()), collapse = ""),
          "\r\n"
        )
      ) %>%
      purrr::reduce(stringr::str_c)

    # combine components into a single string
    string_out <- stringr::str_c(ser_name, ser_time, ser_string)
  }

  # apply the get_ser_string function to each series in the data
  to_save <- in_summary %>%
    dplyr::pull(.data$id) %>%
    purrr::map(get_ser_string) %>%
    purrr::reduce(stringr::str_c)

  # save the contents of the tsd file to a user defined location
  to_save %>% readr::write_file(file)
}


#' Copy a data frame to clipboard (only works on MacOS)
#'
#' @param x tibble (or data frame) to be copied
#' @param dec number of decimals to round numeric columns to (default: 2)
#'
#' @return copy_tbl() returns the input x invisibly
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
