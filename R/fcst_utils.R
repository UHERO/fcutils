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
#'
#' @details This function requires access permission to UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "this is your UDAMAN token"
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
  if(name == "") {
    series <- dplyr::bind_cols(time = seq(lubridate::ymd("2000-01-01"), lubridate::ymd("2010-01-01"), by = "year"), values = rep(NA_real_, 11))
    name <- ser_id
  }
  cat(name, "\n")
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
#' @details This function requires access permission to UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "this is your UDAMAN token"
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
#' @details This function requires access permission to UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "this is your UDAMAN token"
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
# OLD VERSION USING USER NAME AND PASSWORD
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
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() %$% get_var("E{ser_i}_{cnty_i}")
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
#' quarterly_data_example |> dplyr::rename_with(~ rename_udaman(., freq = "M"), .cols = -1)
#' quarterly_data_example |> dplyr::rename_with(rename_udaman, freq = "M", .cols = !time)
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(id = rename_udaman(id, freq = "M")) |>
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

