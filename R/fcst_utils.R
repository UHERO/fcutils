#
# Macroeconomic Forecasting for Hawaii
#
# Utility Functions for Time Series in the Model
#
# Author: Peter Fuleky
# Date: October 1, 2019
# To do: modify time series utilities
# so they accept xts, long and wide tbl (see ma())


# **************************
# data retrieval and manipulation ----
# **************************

#' Download a single series from udaman using series name
#'
#' @param ser_id udaman series name
#' @param expand "true" or "raw" ("true" downloads formatted data, "raw"
#'   downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in
#'   parentheses
#'
#' @return time and data for a single series combined in a tibble
#' @export
#'
#' @examplesIf interactive()
#' get_series_1(ser_id = "VISNS@HI.M")
get_series_1 <- function(ser_id, expand = "true", rename = "compact", descr = FALSE) {
  # API call
  url <- stringr::str_c("https://api.uhero.hawaii.edu/v1.u/series?name=", ser_id, "&expand=", expand, "&u=uhero&nocache")
  req <- httr::GET(url, httr::add_headers(Authorization = stringr::str_c("Bearer ", Sys.getenv("udaman_token"))))
  json <- httr::content(req, as = "text")
  uhero_data <- jsonlite::fromJSON(json)
  # extract series info
  dates <- uhero_data$data$observations$transformationResults$dates[[1]]
  values <- uhero_data$data$observations$transformationResults$values[[1]]
  series <- dplyr::bind_cols(time = lubridate::ymd(dates), values = as.numeric(values))
  name <- uhero_data$data$series$name
  title <- uhero_data$data$series$title
  geo <- uhero_data$data$series$geography$shortName
  colnames(series) <- dplyr::case_when(
    rename == "compact" ~ c("time", name %>%
      stringr::str_replace_all(c("\\.[A-Z]" = "", "@" = "_", "OCUP%" = "OCUPP")) %>%
      {
        if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
      }),
    rename == "full" ~ c("time", name %>%
      stringr::str_replace_all(c("\\.([A-Z])" = "_\\1", "@" = "__", "OCUP%" = "OCUPP")) %>%
      {
        if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
      }),
    rename == "no" ~ c("time", name %>%
      {
        if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
      })
  )
  # return series
  return(series)
}


#' Download a set of series from udaman using series names
#'
#' @param ser_id_vec vector of series names
#' @param format "wide" (default) or "long" or "xts"
#' @param expand "true" (default) or "raw" ("true" downloads formatted data,
#'   "raw" downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param freq if frequency is missing from series names (or want to modify freq
#'   in existing names) specify frequency, e.g. "M".
#' @param descr if TRUE add to the udaman series name the series description in
#'   parentheses
#'
#' @return time and data for all series combined in an object specified by the
#'   format option
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M"))
#' get_series(c("VISNS@HI.M", "VISUSNS@HI.M"), freq = "Q")
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M"), format = "xts")
#' get_series(c("VISNS@HI.M"), format = "xts")
#' get_series(c("VISNS@HI.M"), format = "xts", descr = TRUE)
#' get_series(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#' get_series(c("E_NF__HI_M", "ECT__HI_M", "VAP__HI_W"))
get_series <- function(ser_id_vec, format = "wide", expand = "true", rename = "compact", freq = NULL, descr = FALSE) {
  ser_tbl <- ser_id_vec %>%
    rename_udaman(., freq = freq) %>%
    purrr::map(get_series_1, expand = expand, rename = rename, descr = descr) %>%
    purrr::reduce(dplyr::full_join, by = "time") %>%
    dplyr::arrange(.data$time)
  if (format == "wide") ser_out <- ser_tbl
  if (format == "long") ser_out <- ser_tbl %>% tsbox::ts_long()
  if (format == "xts") {
    ser_out <- ser_tbl %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }
  return(ser_out)
}


#' Download series listed in an export table from udaman
#'
#' @param exp_id export id
#' @param format "wide" (default) or "long" or "xts"
#' @param expand "true" or "raw" ("true" downloads formatted data, "raw"
#'   downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in
#'   parentheses
#'
#' @return time and data for all series combined in a tibble
#' @export
#'
#' @examplesIf interactive()
#' get_series_exp(exp_id = 74)
#' get_series_exp(74, format = "xts")
get_series_exp <- function(exp_id, format = "wide", expand = "true", rename = "compact", descr = FALSE) {
  # API call
  url <- stringr::str_c("https://api.uhero.hawaii.edu/v1.u/package/export?id=", exp_id, "&expand=", expand, "&u=uhero&nocache")
  req <- httr::GET(url, httr::add_headers(Authorization = stringr::str_c("Bearer ", Sys.getenv("udaman_token"))))
  json <- httr::content(req, as = "text")
  uhero_data <- jsonlite::fromJSON(json)
  # extract series info
  dates <- uhero_data$data$seriesObservations$transformationResults %>%
    purrr::map("dates") %>%
    purrr::flatten()
  values <- uhero_data$data$seriesObservations$transformationResults %>%
    purrr::map("values") %>%
    purrr::flatten()
  data_lst <- purrr::map2(dates, values, ~ tibble::tibble(time = .x %>% lubridate::ymd(), value = .y %>% as.numeric()))
  empty_ser <- data_lst %>%
    purrr::map(~ nrow(.x) == 0) %>%
    purrr::reduce(c)
  cat(stringr::str_c("The following series did not contain data: ", uhero_data$data$name[empty_ser], collapse = "\n"))
  data_tbl <- data_lst %>%
    purrr::discard(~ nrow(.x) == 0) %>%
    purrr::reduce(tsbox::ts_c) %>%
    tsbox::ts_wide()
  name <- uhero_data$data$name[!empty_ser]
  title <- uhero_data$data$title[!empty_ser]
  # geo <- NULL
  colnames(data_tbl) <- dplyr::case_when(
    rename == "compact" ~ c("time", name %>%
      stringr::str_replace_all(c("\\.[A-Z]" = "", "@" = "_", "OCUP%" = "OCUPP")) %>%
      {
        # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
        if (descr) stringr::str_c(., " (", title, ")") else .
      }),
    rename == "full" ~ c("time", name %>%
      stringr::str_replace_all(c("\\.([A-Z])" = "_\\1", "@" = "__", "OCUP%" = "OCUPP")) %>%
      {
        # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
        if (descr) stringr::str_c(., " (", title, ")") else .
      }),
    rename == "no" ~ c("time", name %>%
      {
        # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
        if (descr) stringr::str_c(., " (", title, ")") else .
      })
  )
  if (format == "wide") data_out <- data_tbl
  if (format == "long") data_out <- data_tbl %>% tsbox::ts_long()
  if (format == "xts") {
    data_out <- data_tbl %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }
  return(data_out)
}
# get_series_exp <- function(exp_id, format = "wide", save_loc = "data/raw") {
#   url <- "https://udaman.uhero.hawaii.edu/"
#   dn_url <- stringr::str_c("https://udaman.uhero.hawaii.edu/exports/", exp_id, ".csv")
#   session <- rvest::session(url)
#   form <- rvest::html_form(session)[[1]]
#   fl_fm <- rvest::html_form_set(form,
#     `user[email]` = Sys.getenv("udaman_user"),
#     `user[password]` = Sys.getenv("udaman_pwd")
#   )
#   main_page <- rvest::session_submit(session, fl_fm)
#   download <- rvest::session_jump_to(main_page, dn_url)
#   content <- readBin(download$response$content, what = character())[1]
#   data_tbl <- read_csv(file = content) %>%
#     rename(time = date)
#     # rename_with(~str_replace_all(., c("date" = "time", "\\.[A-Z]" = "", "@" = "__", "OCUP%" = "OCUPP")))
#   if (!is.null(save_loc)) write_csv(data_tbl, here(save_loc, basename(dn_url)))
#   if (format == "wide") data_out <- data_tbl
#   if (format == "long") data_out <- data_tbl %>% ts_long()
#   if (format == "xts") data_out <- data_tbl %>% ts_long() %>% ts_xts()
#   return(data_out)
# }


#' Create xts and fill with values
#'
#' @param start date of series start (string: "yyyy-mm-dd")
#' @param end date of series end (string: "yyyy-mm-dd")
#' @param per periodicity of series (string: "quarter", "year")
#' @param val values to fill in (scalar or vector)
#'
#' @return an xts series
#' @export
#'
#' @examples
#' make_xts()
#' make_xts(start = lubridate::ymd("2010-01-01"), per = "quarter", val = 0)
make_xts <- function(start = bnk_start, end = bnk_end, per = "year", val = NA_real_) {
  tibble::tibble(
    time = seq.Date(start, end, by = per),
    value = val
  ) %>%
    tsbox::ts_xts()
}


# #' Splitting of xts matrix to individual xts vectors
# #' DON'T USE!!! POLLUTES GLOBAL ENVIRONMENT
# #'
# #' @param xts_in the xts matrix to be split into individual xts vectors
# #'
# #' @return nothing (silently store split series in global environment)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' get_series_exp(74, save_loc = NULL) |>
# #'   tsbox::ts_long() |>
# #'   tsbox::ts_xts() |>
# #'   explode_xts()
# #' rm(list = ls(pattern = glob2rx("*@HI.Q")))
# #' }
# explode_xts <- function(xts_in) {
#   temp <- xts_in %>%
#     tsbox::ts_tbl() %>%
#     tsbox::ts_wide()
#   for (i in names(temp)[-1]) {
#     assign(i, xts::xts(temp[i], temp %>% dplyr::pull(.data$time)), envir = .GlobalEnv)
#   }
#   return("OK. Done.")
# }


#' Construct a series name from variable components and retrieve the series
#'
#' @param ser_in a variable name (string with substituted expressions)
#' @param env environment where the expression should be evaluated
#'
#' @return variable
#' @export
#'
#' @examplesIf interactive()
#' ser_i <- "_NF"
#' cnty_i <- "HI"
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() %$% get_var("E{ser_i}@{cnty_i}.Q")
get_var <- function(ser_in, env = parent.frame()) {
  return(ser_in %>% stringr::str_glue() %>% get(envir = env, inherits = TRUE))
}


#' Format series names to udaman format (mnemonic@loc.freq)
#'
#' @param ser_in series name (string "mnemonic_loc", "mnemonic__loc_freq")
#' @param freq frequency of the series, required if not contained in the series
#'   name (string "D", "W", "M", "Q", "S", "A")
#'
#' @return series name following udaman convention "mnemonic@loc.freq"
#' @export
#'
#' @examples
#' rename_udaman(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#' rename_udaman(c("E_NF__HI_M", "ECT__HI_M", "VAP__HAW_W"))
#' rename_udaman(c("E_NF@HI.M", "ECT@HI.M", "VAP@HAW.W"))
#' rename_udaman(c("SH_US@HI.M", "SH_JP__HON_M"))
#' @examplesIf interactive()
#' get_series_exp(74) |> dplyr::rename_with(~ rename_udaman(., freq = "M"), .cols = -1)
#' get_series_exp(74) |> dplyr::rename_with(rename_udaman, freq = "M", .cols = !time)
#' get_series_exp(74) |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(id = rename_udaman(id, freq = "M"))
#' get_series_exp(318, rename = "full") |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(id = rename_udaman(id)) |>
#'   tsbox::ts_xts()
rename_udaman <- function(ser_in, freq = NULL) {
  if (!is.character(ser_in)) {
    ser_in <- as.character(ser_in)
  }
  mnemonic <- ser_in %>%
    stringr::str_extract("[[:alnum:]_]+(?=_+HI|_+HON|_+HAW|_+MAU|_+KAU|_+NBI|_+US|_+JP|@)") %>%
    stringr::str_replace("_$", "")
  loc <- ser_in %>%
    {
      ifelse(stringr::str_detect(., "@"),
        stringr::str_extract(., "@HI|@HON|@HAW|@MAU|@KAU|@NBI|@US|@JP"),
        stringr::str_extract(., "_+HI|_+HON|_+HAW|_+MAU|_+KAU|_+NBI|_+US|_+JP")
      )
    } %>%
    stringr::str_replace("_+", "@")
  freq <- if (!is.null(freq)) {
    stringr::str_replace(freq, "^", ".")
  } else {
    ser_in %>%
      stringr::str_extract("_D$|_W$|_M$|_Q$|_S$|_A$|.D$|.W$|.M$|.Q$|.S$|.A$") %>%
      stringr::str_replace("_", ".")
  }
  return(stringr::str_c(mnemonic, loc, freq))
}

#' Save an xts or a wide data frame with time series in tsd format
#'
#' @param x an xts or a (wide) data frame with time series (only M, Q, A
#'   frequency)
#' @param file string denoting the location and name of the output file
#'
#' @return nothing (silently save the contents of the tsd file to a user defined
#'   location)
#' @export
#'
#' @examplesIf interactive()
#' get_series_exp(74) |> write_tsd("out.tsd")
write_tsd <- function(x, file) {
  # convert the xts or wide data frame to tslist
  if ("xts" %in% class(x)) {
    in_list <- x %>%
      tsbox::ts_tbl() %>%
      tidyr::drop_na() %>%
      tsbox::ts_tslist()
  } else if (setequal(c("id", "time", "value"), colnames(x))) {
    in_list <- x %>%
      tidyr::drop_na() %>%
      tsbox::ts_tslist()
  } else {
    in_list <- x %>%
      tsbox::ts_long() %>%
      tidyr::drop_na() %>%
      tsbox::ts_tslist()
  }

  # get summary info about the time series
  in_summary <- in_list %>%
    tsbox::ts_summary()

  # get character strings with the contents of individual series
  get_ser_string <- function(ser_i) {
    # add padding to the name of the series
    ser_name <- stringr::str_c(ser_i, stringr::str_c(rep(" ", 80 - nchar(ser_i)), collapse = ""), "\r\n")

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
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 12 ~ in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(start) %>% lubridate::month() %>% formatC(width = 2, flag = "0"),
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 4 ~ in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(start) %>% lubridate::quarter() %>% formatC(width = 2, flag = "0"),
        TRUE ~ "01"
      ),
      "00",
      # ending year and period
      in_summary %>%
        dplyr::filter(.data$id == ser_i) %>%
        dplyr::pull(.data$end) %>%
        lubridate::year(),
      dplyr::case_when(
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 12 ~ in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(end) %>% lubridate::month() %>% formatC(width = 2, flag = "0"),
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 4 ~ in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(end) %>% lubridate::quarter() %>% formatC(width = 2, flag = "0"),
        TRUE ~ "01"
      ),
      "00",
      # frequency label
      dplyr::case_when(
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 12 ~ "M",
        in_summary %>% dplyr::filter(.data$id == ser_i) %>% dplyr::pull(freq) == 4 ~ "Q",
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
      purrr::pluck(ser_i) %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(value = dplyr::if_else(is.na(.data$value), 1.000000E+0015, .data$value)) %>%
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
      purrr::map(~ stringr::str_c(.x, stringr::str_c(rep(" ", 80 - .x %>% nchar()), collapse = ""), "\r\n")) %>%
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


#' Copy a data frame to clipboard
#'
#' @param x tibble (or data frame) to be copied
#'
#' @return copy_tbl() returns the input x invisibly
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M")) %>% copy_tbl()
copy_tbl <- function(x) {
  readr::write_delim(x, pipe("pbcopy"), delim = "\t")
}


# **************************
# time series utility functions ----
# **************************

#' Interpolate a single series from quarterly to monthly freq
#'
#' @param var_q vector containing a single variable at quarterly freq
#' @param ts_start starting period as c(year, quarter) e.g. c(2001, 1)
#' @param conv_type match the quarterly value via "first", "last", "sum",
#'   "average"
#'
#' @return vector containing a single variable at monthly freq
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' QtoM_1(test1, c(2010, 1), "average")
QtoM_1 <- function(var_q, ts_start, conv_type) {
  var_q_ts <- stats::ts(var_q, frequency = 4, start = ts_start)
  tempdisagg::td(
    formula = var_q_ts ~ 1,
    conversion = conv_type,
    to = "monthly",
    method = "denton-cholette"
  ) %>%
    stats::predict()
}


#' Interpolate a tibble of series from quaterly to monthly freq
#'
#' @param data_q tibble containing variables at quarterly freq the first column
#'   of data_q named "time" contains dates
#' @param conv_type match the quarterly value via "first", "last", "sum",
#'   "average"
#'
#' @return tibble containing variables at monthly freq
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' QtoM(tsbox::ts_tbl(test1), "average")
#' tsbox::ts_frequency(QtoM(tsbox::ts_tbl(test1), "average") |> tsbox::ts_xts())
QtoM <- function(data_q, conv_type) {
  data_q_names <- colnames(data_q)
  data_q_dates <- lubridate::ymd(data_q$time)
  data_q_first <- dplyr::first(data_q_dates)
  data_q_last <- dplyr::last(data_q_dates)
  data_q_start <- c(lubridate::year(data_q_first), lubridate::quarter(data_q_first))
  data_m <- data_q %>%
    dplyr::select(-.data$time) %>%
    purrr::map(QtoM_1, data_q_start, conv_type) %>%
    purrr::reduce(stats::ts.union) %>%
    tibble::as_tibble()
  data_m_dates <- seq(data_q_first, data_q_last + months(2), by = "months") %>%
    tibble::enframe(name = NULL)
  data_m <- dplyr::bind_cols(data_m_dates, data_m) %>%
    dplyr::rename_with(~data_q_names)
  return(data_m)
}


#' Linear interpolation based on aremos command reference page 292
#'
#' @param ser_in the xts series to be interpolated (freq = a)
#' @param aggr interpolation method: aggregate via mean (default) or sum
#'
#' @return interpolated xts series (freq = q)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
AtoQ <- function(ser_in, aggr = "mean") {
  ser_out_name <- names(ser_in)
  ser_out_dates <- tibble::tibble(time = seq.Date(
    from = tsbox::ts_summary(ser_in)$start,
    to = tsbox::ts_summary(ser_in)$end %>% lubridate::ceiling_date(unit = "year") %>% lubridate::rollback(),
    by = "quarter"
  ))
  ser_out <- dplyr::left_join(ser_out_dates, ser_in %>% tsbox::ts_tbl()) %>%
    tidyr::fill(.data$value) %>%
    tsbox::ts_xts() %>%
    magrittr::set_names(ser_out_name)
  dat_start <- tsbox::ts_summary(ser_out)$start
  dat_end <- tsbox::ts_summary(ser_out)$end
  increment <- (tsbox::ts_lag(ser_out, -4) - ser_out) / 4
  increment <- increment %>% tsbox::ts_bind(tsbox::ts_lag(increment, 4)[stringr::str_c(dat_end - months(9), "/", dat_end)])
  ser_out[p(dat_start + months(3), dat_end)] <- (as.numeric(ser_out[dat_start]) + tsbox::ts_lag(increment, 1)[p(dat_start + months(3), dat_end)] %>% cumsum()) %>% as.numeric()
  ser_out <- ser_out - 1.5 * increment
  if (aggr != "mean") ser_out <- ser_out / 4
  colnames(ser_out) <- colnames(ser_in) %>%
    gsub(".SOLA", ".SOLQ", .) %>%
    gsub(".A", ".Q", .)
  return(ser_out)
}


#' Conversion from quarterly to annual frequency
#'
#' @param ser_in the xts series to be converted (freq = q)
#' @param	aggr aggregate via mean (default) or sum
#'
#' @return converted xts series (freq = a)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' test2 <- QtoA(test1) # for stock type variables mean, for flow type variables sum
#' print(test1)
#' print(cbind(`ncen@us.sola`, test2))
QtoA <- function(ser_in, aggr = "mean") {
  ser_out <- tsbox::ts_frequency(ser_in, to = "year", aggregate = aggr)
  colnames(ser_out) <- colnames(ser_in) %>%
    gsub(".SOLQ", ".SOLA", .) %>%
    gsub(".Q", ".A", .)
  return(ser_out)
}


#' Year to date sum or average
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param avg if true, the year to date average, if false, the year to date sum
#'
#' @return a long tibble of time series containing year to date sum or average
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M")) |>
#'   tsbox::ts_long() |>
#'   ytd_cum()
ytd_cum <- function(long_tbl_in, avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    dplyr::mutate(yr = lubridate::floor_date(.data$time, "year")) %>%
    dplyr::group_by(.data$id, .data$yr) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$yr)
  return(long_tbl_out)
}


#' Year to date growth rate
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param avg if true, the year to date average, if false, the year to date sum
#'   for calculation
#'
#' @return a long tibble of time series containing year to date growth rate
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M")) |>
#'   tsbox::ts_long() |>
#'   ytd_gr() |>
#'   tail()
ytd_gr <- function(long_tbl_in, avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    ytd_cum(avg) %>%
    tsbox::ts_pcy()
  return(long_tbl_out)
}


#' Month to date sum or average
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param avg if true, the year to date average, if false, the year to date sum
#'
#' @return a long tibble of time series containing year to date sum or average
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISPNS@HI.D", "VAPNS@HI.D")) |>
#'   tsbox::ts_long() |>
#'   mtd_cum()
#' test <- get_series("VAPNS@HI.D") |>
#'   tsbox::ts_long() |>
#'   mtd_cum()
#' test %ts/% tsbox::ts_lag(test, "3 years") |> tail()
mtd_cum <- function(long_tbl_in, avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    dplyr::mutate(yrmo = lubridate::floor_date(.data$time, "month")) %>%
    dplyr::group_by(.data$id, .data$yrmo) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$yrmo)
  return(long_tbl_out)
}


#' Month to date growth rate
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param avg if true, the year to date average, if false, the year to date sum
#'   for calculation
#'
#' @return a long tibble of time series containing year to date growth rate
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISPNS@HI.D", "VAPNS@HI.D")) |>
#'   tsbox::ts_long() |>
#'   mtd_gr() |>
#'   tail()
mtd_gr <- function(long_tbl_in, avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    mtd_cum(avg) %>%
    tsbox::ts_pcy()
  return(long_tbl_out)
}


#' Period to date sum or average
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param per unit of time supplied to floor_date() (for ytd per = "year"
#'   (default), for mtd per = "month")
#' @param avg if true (default), the year to date average, if false, the year to
#'   date sum
#'
#' @return a long tibble of time series containing year to date sum or average
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M")) |>
#'   tsbox::ts_long() |>
#'   ptd_cum()
ptd_cum <- function(long_tbl_in, per = "year", avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    dplyr::mutate(time_per = lubridate::floor_date(.data$time, per)) %>%
    dplyr::group_by(.data$id, .data$time_per) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$time_per)
  return(long_tbl_out)
}


#' Period to date growth rate
#'
#' @param long_tbl_in a long tibble of time series (produced by ts_long() for
#'   example)
#' @param per unit of time supplied to floor_date() (for ytd per = "year"
#'   (default), for mtd per = "month")
#' @param lag_length period over which growth is calculated (e.g. "1 year"
#'   (default), "3 years", etc. See ?ts_lag() for options)
#' @param avg if true, the year to date average, if false, the year to date sum
#'   for calculation
#'
#' @return a long tibble of time series containing year to date growth rate
#' @export
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M")) |>
#'   tsbox::ts_long() |>
#'   ptd_gr() |>
#'   tail()
#' get_series("VAPNS@HI.D") |>
#'   tsbox::ts_long() |>
#'   ptd_gr(per = "month", lag_length = "3 years") |>
#'   tail()
ptd_gr <- function(long_tbl_in, per = "year", lag_length = "1 year", avg = TRUE) {
  long_tbl_out <- long_tbl_in %>%
    ptd_cum(per, avg) %>%
    {
      (. %ts/% tsbox::ts_lag(., lag_length) %ts-% 1) %ts*% 100
    }
  return(long_tbl_out)
}


#' Convert annualized growth to quarterly growth
#'
#' @param ser_in the series containing annualized growth (in percent)
#'
#' @return series containing quarterly growth (in percent)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' tsbox::ts_c(test1 |> tsbox::ts_pca() |> pca_to_pc(), test1 |> tsbox::ts_pc())
pca_to_pc <- function(ser_in) {
  ((1 + ser_in / 100)^0.25 - 1) * 100
}


#' Convert quarterly growth to annualized growth
#'
#' @param ser_in series containing quarterly growth (in percent)
#'
#' @return the series containing annualized growth (in percent)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' tsbox::ts_c(test1 |> tsbox::ts_pc() |> pc_to_pca(), test1 |> tsbox::ts_pca())
pc_to_pca <- function(ser_in) {
  ((1 + ser_in / 100)^4 - 1) * 100
}


#' Calculate multi-period average growth
#'
#' @param ser_in name of xts series for which growth is calculated
#' @param lag_in length of period over which growth is calculated
#'
#' @return series containing the average growth of ser_in (in percent)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' tsbox::ts_c(pcmp(`ncen@us.sola`, lag_in = 3), tsbox::ts_pc(`ncen@us.sola`))
#' tsbox::ts_c(
#'   pcmp(test1, lag_in = 4),
#'   tsbox::ts_pcy(test1),
#'   tsbox::ts_pca(test1),
#'   tsbox::ts_pc(test1)
#' )
pcmp <- function(ser_in, lag_in = 1) {
  ser_in <- tsbox::ts_xts(ser_in)
  ser_out <- (((ser_in / tsbox::ts_lag(ser_in, lag_in))^(1 / lag_in)) - 1) * 100
  return(ser_out)
}


#' Find the date of the first observation (NAs are dropped)
#'
#' @param ser_in an xts series
#'
#' @return date associated with first observation
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2017/2021"] <- c(325511184, 327891911, 330268840, 332639102, 334998398)
#' find_start(`ncen@us.sola`)
find_start <- function(ser_in) {
  # ser_in %>% na.omit() %>% start()
  ser_in %>%
    tsbox::ts_summary() %>%
    dplyr::pull(.data$start)
}


#' Find the date of the last observation (NAs are dropped)
#'
#' @param ser_in an xts series
#'
#' @return date associated with last observation
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2060, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2018"] <- c(323127513, 325511184, 327891911)
#' find_end(`ncen@us.sola`)
find_end <- function(ser_in) {
  # ser_in %>% na.omit() %>% end()
  ser_in %>%
    tsbox::ts_summary() %>%
    dplyr::pull(.data$end)
}


#' Concatenate dates to obtain period
#'
#' @param dat1 date of period start (string: yyyy-mm-dd)
#' @param dat2 date of period end (string: yyyy-mm-dd)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' p("2010-01-01", "2020-01-01")
#' p(2010, 2020) # for annual period only
p <- function(dat1 = "", dat2 = "") {
  stringr::str_c(dat1, dat2, sep = "/")
}


#' Concatenate dates formatted as yyyyMm or yyyy.m to obtain period
#'
#' @param dat1 date of period start (string: yyyyMm or yyyy.m)
#' @param dat2 date of period end (string: yyyyMm or yyyy.m)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' pm("2010M1", "2020M4")
#' pm(2010.1, 2020.4)
#' pm(2010.1, )
#' pm(, 2010.1)
pm <- function(dat1 = "", dat2 = "") {
  stringr::str_c(if (dat1 != "") dat1 %>% lubridate::ym(), "/", if (dat2 != "") dat2 %>% lubridate::ym())
}


#' Concatenate dates formatted as yyyyQq or yyyy.q to obtain period
#'
#' @param dat1 date of period start (string: yyyyQq or yyyy.q)
#' @param dat2 date of period end (string: yyyyQq or yyyy.q)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' pq("2010Q1", "2020Q4")
#' pq(2010.1, 2020.4)
#' pq(2010.1, )
#' pq(, 2010.1)
pq <- function(dat1 = "", dat2 = "") {
  stringr::str_c(if (dat1 != "") dat1 %>% lubridate::yq(), "/", if (dat2 != "") dat2 %>% lubridate::yq())
}


#' Concatenate date formatted as yyyyQq or yyyy.q to obtain single period
#' DON'T USE!!! USE lubridate::yq() INSTEAD
#'
#' @param dat1 date of period start (string: yyyyQq or yyyy.q)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' pq_1("2010Q1")
#' pq_1(2010.1)
pq_1 <- function(dat1 = "") {
  if (dat1 != "") dat1 %>% lubridate::yq()
}
#' Concatenate dates formatted as yyyy to obtain period
#'
#' @param dat1 year of period start (string or numeric: yyyy)
#' @param dat2 year of period end (string or numeric: yyyy)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' py("2010", "2020")
#' py(2010, 2020)
#' py(2010, )
#' py(, 2010)
py <- function(dat1 = "", dat2 = "") {
  stringr::str_c(if (dat1 != "") stringr::str_c(dat1, "-01-01"), "/", if (dat2 != "") stringr::str_c(dat2, "-01-01"))
}


#' Convert dates from yyyy-mm-dd to yyyyQqq format
#'
#' @param x dates (string: yyyy-mm-dd)
#'
#' @return formatted dates (string: yyyyQqq)
#' @export
#'
#' @examples
#' ymd_to_yQq(c("2010-01-01", "2020-10-01"))
#' ymd_to_yQq(c("2010-01-01", "2020-10-01")) |> lubridate::yq()
ymd_to_yQq <- function(x) {
  x %>%
    lubridate::quarter(type = "year.quarter") %>%
    stringr::str_replace("\\.", "Q")
}


#' Convert period in quarters to period in months
#'
#' @param nr_quarters number of quarters in period (integer)
#'
#' @return number of months in period
#' @export
#'
#' @examples
#' qtrs(3)
#' lubridate::ymd("2020-01-01") + qtrs(3)
qtrs <- function(nr_quarters) {
  nr_quarters * months(3)
}


#' Calculate number of quarters between two dates yyyyQq or yyyy.q
#'
#' @param dat1 date of period start (string: yyyyQq or yyyy.q)
#' @param dat2 date of period end (string: yyyyQq or yyyy.q)
#'
#' @return numeric length of date range in quarters
#' @export
#'
#' @examples
#' nqtrs("2010Q1", "2020Q4")
#' nqtrs(2010.1, 2020.4)
nqtrs <- function(dat1 = "", dat2 = "") {
  xts::nquarters(make_xts(start = lubridate::yq(dat1), end = lubridate::yq(dat2), per = "quarters"))
}


#' Backward looking moving average
#'
#' @param ser series (xts, long or wide tbl)
#' @param ord numeric order (window length) of moving average
#'
#' @return object with same class as input containing moving average
#' @export
#'
#' @examples
#' test <- make_xts(
#'   start = lubridate::ymd("2010-01-01"), end = lubridate::ymd("2015-01-01"),
#'   per = "quarter", val = 0:20
#' ) |>
#'   magrittr::set_names(c("test"))
#' test <- test |>
#'   cbind(test * 2) |>
#'   magrittr::set_names(c("test1", "test2"))
#' test |> ma(3)
#' test |>
#'   tsbox::ts_tbl() |>
#'   ma(3)
#' test |>
#'   tsbox::ts_tbl() |>
#'   tsbox::ts_wide() |>
#'   ma(3)
ma <- function(ser, ord) {
  if ("xts" %in% class(ser)) { # xts
    ser %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      dplyr::mutate(dplyr::across(-.data$time, ~ slider::slide_dbl(.x, mean, .before = ord - 1, .complete = TRUE))) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  } else if (setequal(c("id", "time", "value"), colnames(ser))) { # long tbl
    ser %>%
      tsbox::ts_wide() %>%
      dplyr::mutate(dplyr::across(-.data$time, ~ slider::slide_dbl(.x, mean, .before = ord - 1, .complete = TRUE))) %>%
      tsbox::ts_long()
  } else { # wide tbl
    ser %>%
      dplyr::mutate(dplyr::across(-.data$time, ~ slider::slide_dbl(.x, mean, .before = ord - 1, .complete = TRUE)))
  }
}


# **************************
# plotting functions ----
# **************************
#' Interactive plot with level and growth rate
#'
#' @param ser time series to plot (e.g. history, oldsol, sol)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#' @param gr_1 only show growth for the first series (default)
#'
#' @return a dygraph plot
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' `ncen@us.oldsola` <- `ncen@us.sola`
#' `ncen@us.oldsola`["2020/2021"] <- c(352639102, 374998398)
#' `ncen@us.a` <- `ncen@us.sola`
#' `ncen@us.a`["2020/2021"] <- NA
#' test1 <- tsbox::ts_tslist(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`)) |>
#'   purrr::map(AtoQ) |>
#'   purrr::reduce(tsbox::ts_c) |>
#'   magrittr::set_names(c("ncen@us.sola", "ncen@us.oldsola", "ncen@us.a"))
#' plot_1(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`), rng_start = "2017-01-01")
#' plot_1(test1, rng_start = "2017-01-01", gr_1 = FALSE)
#' test1 <- AtoQ(`ncen@us.sola`)
#' plot_1(`ncen@us.sola`, rng_start = "2017-01-01")
#' plot_1(test1, rng_start = "2017-01-01")
#' @examplesIf interactive()
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   magrittr::extract(, c("E_NF@HI.Q", "ECT@HI.Q", "EMN@HI.Q")) |>
#'   plot_1()
plot_1 <- function(ser, rng_start = as.character(Sys.Date() - lubridate::years(15)), rng_end = as.character(Sys.Date()), height = 300, width = 900, yoy_gr = TRUE, gr_1 = TRUE) {
  ser_names <- ser %>%
    tsbox::ts_xts() %>%
    names()

  ser_names_pct <- if (gr_1) stringr::str_glue("{ser_names}%")[1] else stringr::str_glue("{ser_names}%")

  ser_plot <- ser %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_c(., tsbox::ts_pcy(.)) else tsbox::ts_c(., tsbox::ts_pc(.))
    } %>%
    magrittr::extract(, 1:length(c(ser_names, ser_names_pct))) %>%
    magrittr::set_names(c(ser_names, ser_names_pct)) %>%
    tsbox::ts_dygraphs(main = ser_names[1], group = "comp", height = height, width = width) %>%
    dygraphs::dyAxis("y", label = "% change") %>%
    dygraphs::dyAxis("y2", label = "level", drawGrid = FALSE, independentTicks = TRUE) %>%
    {
      if (length(ser_names_pct) > 1) dygraphs::dyGroup(., ser_names_pct, axis = "y") %>% dygraphs::dyMultiColumnGroup(ser_names_pct) else dygraphs::dyBarSeries(., ser_names_pct, axis = "y")
    } %>%
    {
      if (length(ser_names) > 1) dygraphs::dyGroup(., ser_names, strokeWidth = 2, axis = "y2") else dygraphs::dySeries(., ser_names, strokeWidth = 3, axis = "y2")
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(colors = uhero_colors_light[1:length(ser_names_pct)] %>% c(uhero_colors[1:length(ser_names)])) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(dateWindow = c(rng_start, rng_end), height = 30, strokeColor = "red")

  # render the dygraphs objects using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Interactive lineplot with two axes
#'
#' @param ser time series to plot (e.g. history, oldsol, sol)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#'
#' @return a dygraph plot
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' `ncen@us.oldsola` <- `ncen@us.sola`
#' `ncen@us.oldsola`["2020/2021"] <- c(352639102, 374998398)
#' `ncen@us.a` <- `ncen@us.sola`
#' `ncen@us.a`["2020/2021"] <- NA
#' test1 <- tsbox::ts_tslist(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`)) |>
#'   purrr::map(AtoQ) |>
#'   purrr::reduce(tsbox::ts_c) |>
#'   magrittr::set_names(c("ncen@us.sola", "ncen@us.oldsola", "ncen@us.a"))
#' plot_2ax(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`), rng_start = "2017-01-01")
#' plot_2ax(test1, rng_start = "2017-01-01")
#' @examplesIf interactive()
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   magrittr::extract(, c("E_NF@HI.Q", "ECT@HI.Q", "EMN@HI.Q")) |>
#'   plot_2ax()
plot_2ax <- function(ser, rng_start = as.character(Sys.Date() - lubridate::years(15)), rng_end = as.character(Sys.Date()), height = 300, width = 900) {
  ser_names <- ser %>%
    tsbox::ts_xts() %>%
    names()

  ser_plot <- ser %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(main = ser_names[1] %>% stringr::str_replace_all("@.*", ""), group = "comp", height = height, width = width) %>%
    dygraphs::dyAxis("y", label = "series 1") %>%
    dygraphs::dyAxis("y2", label = "series 2+", drawGrid = FALSE, independentTicks = TRUE) %>%
    {
      if (length(ser_names[-1]) > 1) dygraphs::dyGroup(., ser_names[-1], strokeWidth = 2, axis = "y") else dygraphs::dySeries(., ser_names[-1], strokeWidth = 3, axis = "y")
    } %>%
    {
      dygraphs::dySeries(., ser_names[1], strokeWidth = 3, axis = "y2")
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(dateWindow = c(rng_start, rng_end), height = 30, strokeColor = "red")

  # render the dygraphs objects using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Interactive plot with level and growth rate for forecast series
#'
#' @param ser time series to plot (min 1, max 3) (e.g. current fcst, old fcst, history)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#'
#' @return a dygraph plot
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' `ncen@us.oldsola` <- `ncen@us.sola`
#' `ncen@us.oldsola`["2020/2021"] <- c(352639102, 374998398)
#' `ncen@us.a` <- `ncen@us.sola`
#' `ncen@us.a`["2020/2021"] <- NA
#' test1 <- tsbox::ts_tslist(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`)) |>
#'   purrr::map(AtoQ) |>
#'   purrr::reduce(tsbox::ts_c) |>
#'   magrittr::set_names(c("ncen@us.sola", "ncen@us.oldsola", "ncen@us.a"))
#' plot_fc(tsbox::ts_c(`ncen@us.sola`, `ncen@us.oldsola`, `ncen@us.a`), rng_start = "2017-01-01")
#' plot_fc(test1, rng_start = "2017-01-01")
#' test1 <- AtoQ(`ncen@us.sola`)
#' plot_fc(`ncen@us.sola`, rng_start = "2017-01-01")
#' plot_fc(test1, rng_start = "2017-01-01")
#' @examplesIf interactive()
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   magrittr::extract(, c("E_NF@HI.Q", "ECT@HI.Q", "EMN@HI.Q")) |>
#'   plot_fc()
plot_fc <- function(ser, rng_start = as.character(Sys.Date() - lubridate::years(15)), rng_end = as.character(Sys.Date()), height = 300, width = 900, yoy_gr = TRUE) {
  ser_names <- ser %>%
    tsbox::ts_xts() %>%
    names()
  ser_plot <- ser %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_c(., tsbox::ts_pcy(.[, 1])) else tsbox::ts_c(., tsbox::ts_pc(.[, 1]))
    } %>%
    magrittr::set_names(c(ser_names, stringr::str_glue("{ser_names[1]}%"))) %>%
    tsbox::ts_dygraphs(main = ser_names[1], group = "comp", height = height, width = width) %>%
    dygraphs::dyAxis("y", label = "level") %>%
    dygraphs::dyAxis("y2", label = "% Chg", drawGrid = FALSE, independentTicks = TRUE) %>%
    dygraphs::dySeries(stringr::str_glue("{ser_names[1]}"), axis = "y", strokeWidth = 2, color = uhero_colors[1]) %>%
    {
      if (length(ser_names) > 1) dygraphs::dySeries(., stringr::str_glue("{ser_names[2]}"), axis = "y", strokePattern = "dashed", strokeWidth = 2, color = uhero_colors[2]) else .
    } %>%
    {
      if (length(ser_names) > 2) dygraphs::dySeries(., stringr::str_glue("{ser_names[3]}"), axis = "y", strokePattern = "dashed", strokeWidth = 2, color = uhero_colors[3]) else .
    } %>%
    dygraphs::dySeries(stringr::str_glue("{ser_names[1]}%"), axis = "y2", stepPlot = TRUE, fillGraph = TRUE, color = uhero_colors[1]) %>%
    dygraphs::dyRangeSelector(dateWindow = c(rng_start, rng_end), height = 30, strokeColor = "red") %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE)

  # render the dygraphs objects using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Two-panel plot of levels and growth rates
#'
#' @param sers a vector of series to plot
#' @param rng_start start of the zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#' @param gr_bar show bars or line (default) for the growth series
#'
#' @return a list with two dygraph plots (level, index, growth)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' plot_comp_2(tsbox::ts_c(`ncen@us.sola`, test1), rng_start = "2017-01-01")
#' @examplesIf interactive()
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   magrittr::extract(, c("E_NF@HI.Q", "ECT@HI.Q", "EMN@HI.Q")) |>
#'   plot_comp_2()
plot_comp_2 <- function(sers, rng_start = as.character(Sys.Date() - lubridate::years(15)), rng_end = as.character(Sys.Date()), height = 300, width = 900, yoy_gr = TRUE, gr_bar = FALSE) {
  ser_names <- sers %>%
    tsbox::ts_xts() %>%
    names()
  plot_level <-
    sers %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(main = "Level", group = "comp", height = height, width = width) %>%
    dygraphs::dyLegend(width = width * 0.90) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    sers %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_pcy(.) else tsbox::ts_pc(.)
    } %>%
    tsbox::ts_dygraphs(main = "Growth", group = "comp", height = height, width = width) %>%
    {
      if (gr_bar) dygraphs::dyBarChart(.) else .
    } %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2")) %>%
    dygraphs::dyRangeSelector(dateWindow = c(rng_start, rng_end), height = 30, strokeColor = "red")

  # render the dygraphs objects using htmltools
  list(plot_level, plot_growth) %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Three-panel plot of levels, index, and growth rates
#'
#' @param sers a vector of series to plot
#' @param indx_start base period for the indexed series ("YYYY-MM-DD")
#' @param rng_start start of the zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#' @param gr_bar show bars or line (default) for the growth series
#'
#' @return a list with three dygraph plots (level, index, growth)
#' @export
#'
#' @examples
#' `ncen@us.sola` <- ts(NA_real_, start = 2016, end = 2021, freq = 1) |> tsbox::ts_xts()
#' `ncen@us.sola`["2016/2021"] <- c(323127513, 325511184, 327891911, 330268840, 332639102, 334998398)
#' test1 <- AtoQ(`ncen@us.sola`)
#' plot_comp_3(tsbox::ts_c(`ncen@us.sola`, test1), rng_start = "2017-01-01")
#' @examplesIf interactive()
#' get_series_exp(74, rename = "no") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   magrittr::extract(, c("E_NF@HI.Q", "ECT@HI.Q", "EMN@HI.Q")) |>
#'   plot_comp_3()
plot_comp_3 <- function(sers, indx_start = as.character(Sys.Date() - lubridate::years(15)), rng_start = as.character(Sys.Date() - lubridate::years(15)), rng_end = as.character(Sys.Date()), height = 300, width = 900, yoy_gr = TRUE, gr_bar = FALSE) {
  ser_names <- sers %>%
    tsbox::ts_xts() %>%
    names()
  plot_level <-
    sers %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(main = "Level", group = "comp", height = height, width = width) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  # plot_level[["elementId"]] <- ser_names %>% extract(1) %>% str_extract("^.*@")
  plot_index <-
    sers %>%
    tsbox::ts_xts() %>%
    tsbox::ts_index(base = indx_start) %>%
    tsbox::ts_dygraphs(main = "Index", group = "comp", height = height, width = width) %>%
    # dygraphs::dyRebase(value = 100) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    sers %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_pcy(.) else tsbox::ts_pc(.)
    } %>%
    tsbox::ts_dygraphs(main = "Growth", group = "comp", height = height, width = width) %>%
    {
      if (gr_bar) dygraphs::dyBarChart(.) else .
    } %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2")) %>%
    dygraphs::dyRangeSelector(dateWindow = c(rng_start, rng_end), height = 30, strokeColor = "red")

  # render the dygraphs objects using htmltools
  list(plot_level, plot_index, plot_growth) %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


# **************************
# utility functions for model development ----
# **************************
#' Parse lm() output and convert into bimets equation (GETS model development)
#'
#' @param model a model estimated by lm() (lm object)
#' @param ... arguments to format the coefficients e.g. digits = 3
#'
#' @return a character vector containing the estimated equation (1) and bimets components (2:4)
#' @export
#'
#' @examplesIf interactive()
#' # this function combines coefficient estimates and variable names into an equation
#' # in vector element 1 and into bimets components in vector elements 2-4.
#' # https://stats.stackexchange.com/questions/63600/
#' # how-to-translate-the-results-from-lm-to-an-equation
#' data("UKDriverDeaths", package = "datasets")
#' uk <- log10(UKDriverDeaths)
#' dfm <- dynlm::dynlm(uk ~ L(uk, 1:3) + L(log(uk), c(5:6, 12)))
#' model_equation(dfm)
#' # (1) "uk = - 0.12255631 + 0.42870091 * L(uk, 1:3)1 + 0.06306114 * L(uk, 1:3)2 - 0.09778518 *
#' # L(uk, 1:3)3 + 0.37480999 * L(log(uk), c(5:6, 12))5 - 0.22709846 * L(log(uk), c(5:6, 12))6 +
#' # 1.62340449 * L(log(uk), c(5:6, 12))12"
#' # (2) "BEHAVIORAL> uk"
#' # (3) "EQ> uk = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 2) + b3 * TSLAG(uk, 3) + b4 *
#' # TSLAG(LOG(uk), 5) + b5 * TSLAG(LOG(uk), 6) + b6 * TSLAG(LOG(uk), 12)"
#' # (4) "COEFF> b0 b1 b2 b3 b4 b5 b6"
#' ## regression on multiple lags in a single L() call
#' dfm <- dynlm::dynlm(d(log(uk)) ~ L(uk, c(1, 11, 12)), start = c(1975, 1), end = c(1982, 12))
#' model_equation(dfm)
#' # (1) "d(log(uk)) = 0.1018542 - 0.2379287 * L(uk, c(1, 11, 12))1 + 0.0368355 *
#' # L(uk, c(1, 11, 12))11 + 0.1689896 * L(uk, c(1, 11, 12))12"
#' # (2) "BEHAVIORAL> TSDELTA_LOG_uk"
#' # (3) "EQ> TSDELTA(LOG(uk)) = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 11) + b3 * TSLAG(uk, 12)"
#' # (4) "COEFF> b0 b1 b2 b3"
model_equation <- function(model, ...) { #   model =  est_lm   {model_equation(est_lm)[2:4]}
  format_args <- list(...)

  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- dplyr::case_when(
    model_coeff_sign == -1 ~ " - ",
    model_coeff_sign == 1 ~ " + ",
    model_coeff_sign == 0 ~ " + "
  )

  # model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
  # model_eqn <- paste(strsplit(as.character(model$full_formula), "~")[[2]], # 'y'
  model_eqn <- paste(
    strsplit(as.character(model$terms), "~")[[2]], # 'y'
    "=",
    paste(dplyr::if_else(model_coeff[1] < 0, "- ", ""),
      do.call(format, format_args)[1],
      paste(model_coeff_prefix[-1],
        do.call(format, format_args)[-1],
        " * ",
        names(model_coeff[-1]),
        sep = "", collapse = ""
      ),
      sep = ""
    )
  )

  model_eqn_bim <- paste(
    strsplit(as.character(model$terms), "~")[[2]], # 'y'
    "=",
    paste("b0",
      paste(paste(" + b", 1:length(model_coeff_prefix[-1]), sep = ""),
        " * ",
        names(model_coeff[-1]),
        sep = "", collapse = ""
      ),
      sep = ""
    )
  )

  # model_eqn_beh <- stringr::str_extract(model_eqn_bim, "\\w*") %>% # extract the target variable name
  model_eqn_beh <- stringr::str_extract(model_eqn_bim, "[_.\\(\\)[:alnum:]]+") %>% # extract the target variable name
    gsub("DL_([_.[:alnum:]]+)", "TSDELTALOG(\\1)", .) %>% # replace DL_ with TSDELTALOG()
    gsub("L_([_.[:alnum:]]+)", "LOG(\\1)", .) %>% # replace L_ with LOG()
    gsub("D_([_.[:alnum:]]+)", "TSDELTA(\\1)", .) %>% # replace D_ with TSDELTA()
    gsub("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)", .) %>% # replace log() with LOG()
    gsub("d\\(([_., \\(\\)[:alnum:]]+)\\)", "TSDELTA(\\1)", .) %>% # dynlm::d() = diff()
    gsub("[\\(]+", "_", .) %>% # drop parentheses from equation name
    gsub("[., \\)[:digit:]]+", "", .) # drop parentheses and extra items from equation name
  model_eqn_bim <- model_eqn_bim %>%
    gsub("DL_([_.[:alnum:]]+)", "TSDELTALOG(\\1)", .) %>% # replace DL_ with TSDELTALOG()
    gsub("L_([_.[:alnum:]]+)", "LOG(\\1)", .) %>% # replace L_ with LOG()
    gsub("D_([_.[:alnum:]]+)", "TSDELTA(\\1)", .) %>% # replace D_ with TSDELTA()
    gsub("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)", .) %>% # replace log() with LOG()
    gsub("d\\(([_., \\(\\)[:alnum:]]+)\\)", "TSDELTA(\\1)", .) %>% # dynlm::d() = diff()
    gsub("lag\\(([_., \\(\\)[:alnum:]]+)\\)", "TSLAG(\\1)", .) # lag() with TSLAG()
  model_eqn_bim <- gsub("([_[:alpha:]]+)(\\.)([[:digit:]]{1,2})", "TSLAG(\\1, \\3)", model_eqn_bim) # replace dot notation for lags with TSLAG()
  model_eqn_bim <- gsub("L\\(([_\\(\\) [:alpha:]]+)([, ]+)([:c\\(, [:digit:]\\)]+)\\)([[:digit:]]{1,2})", "TSLAG(\\1, \\4)", model_eqn_bim) # dealing with dynlm::L(x, 1:4) or dynlm::L(x, c(2,4))
  model_eqn_bim <- gsub("L\\(([_., \\(\\)[:alnum:]]+)\\)", "TSLAG(\\1)", model_eqn_bim) # dynlm::L(x, k) = lag(x, -k)
  model_eqn_coe <- stringr::str_extract_all(model_eqn_bim, "(b[[:digit:]]+)", simplify = TRUE) %>% paste(collapse = " ") # # extract coefficients
  model_eqn_beh <- gsub("^", "BEHAVIORAL> ", model_eqn_beh) # add a line for BEHAVIORAL>
  model_eqn_bim <- gsub("^", "EQ> ", model_eqn_bim) # add EQ> to start of line
  model_eqn_coe <- gsub("^", "COEFF> ", model_eqn_coe) # add COEFF> to start of line

  return(c(model_eqn, model_eqn_beh, model_eqn_bim, model_eqn_coe))
}


#' Parse gets output and extract underlying data (GETS model development)
#'
#' @param model_in a model estimated by arx, isat, or getsm
#'
#' @return an xts containing the model variables
#' @export
#'
#' @examplesIf interactive()
#' # save the data associated with a gets model
extract_data <- function(model_in) {
  data_out <- gets::eviews(model_in, print = FALSE, return = TRUE)$data %>%
    dplyr::select(-c) %>%
    dplyr::rename(!!rlang::sym(.data$yvar_name) := "y") %>%
    dplyr::rename_with(~ stringr::str_replace(., "ar", stringr::str_glue("{yvar_name}."))) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., c("iis" = "IIS_", "sis" = "SIS_"))) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., "-", "_")) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  return(data_out)
}


#' Update a bimets model with new/modified equations
#'
#' @param model_1 original estimated bimets model
#' @param model_2 bimets model containing updates (only updated equations need
#'   to be estimated)
#' @param eqList names of updated behavioral equations (vector of strings),
#'   others taken from model_1 (equations missing from model_2 are removed)
#'
#' @return estimated bimets model containing updates
#' @export
#'
#' @examplesIf interactive()
#' update_eqs(scen_model_1_est, scen_model_2_est, c("E_NF_AT_HI_Q", "Y_R_AT_HI_Q"))
update_eqs <- function(model_1, model_2, eqList) {
  # initialize the result with model_2 (contains all equations we want)
  scen_model_est <- model_2

  # equations to keep from original model_1 (if they are present in the updated model_2)
  keep_eqList <- model_1$behaviorals %>%
    names() %>%
    intersect(model_2$behaviorals %>% names()) %>%
    setdiff(eqList)

  # keep the unmodified behavioral equations from the original model_1
  scen_model_est$behaviorals[keep_eqList] <- model_1$behaviorals[keep_eqList]

  return(scen_model_est)
}


# **************************
# other utility functions ----
# **************************

#' In place addition
#'
#' Warning: Typing x %+=% y/2 returns x <- (x + y)/2.
#' Adding parentheses, i.e. x %+=% (y/2) solves the problem.
#'
#' @param e1 first addend (and returned sum)
#' @param e2 second addend
#'
#' @return sum of the two addends replacing the values in the first addend
#' @export
#'
#' @examplesIf interactive()
#' add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] <- add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] +
#'   c(0.01, -0.04, rep(-0.025, 4))
#' add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] %+=% c(0.01, -0.04, rep(-0.025, 4)) # easier on the eye
`%+=%` <- function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))
