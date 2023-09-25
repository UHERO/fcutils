#
# Macroeconomic Forecasting for Hawaii
#
# Utility Functions for Time Series in the Model
#
# Author: Peter Fuleky
# Date: October 1, 2019
# To do:


# **************************
# data retrieval and manipulation ----
# **************************


#' Download a single series from udaman using series name
#'
#' @param ser_id udaman series name (character)
#' @param expand "true" or "raw" ("true" downloads formatted data, "raw" downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#'
#' @return time and data for a single series combined in a tibble
#'
#' @details This function requires access permission to UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "this is your UDAMAN token"
#'
#' @noRd
#' @examplesIf interactive()
#' get_series_1(ser_id = "VISNS@HI.M")
get_series_1 <- function(ser_id, expand = "true", rename = "compact", descr = FALSE) {
  if (is.null(Sys.getenv("udaman_token"))) stop("UDAMAN token is not available in .Renviron")
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
  if (name == "") {
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
#' @param ser_id_vec vector of series names (character)
#' @param format "wide" (default) or "long" or "xts"
#' @param expand "true" (default) or "raw" ("true" downloads formatted data, "raw" downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param freq if frequency is missing from series names (or want to modify freq
#'   in existing names) specify frequency (character), e.g. "M".
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
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
#' @param exp_id export id (character or numeric)
#' @param format "wide" (default) or "long" or "xts"
#' @param expand "true" or "raw" ("true" downloads formatted data, "raw" downloads raw units)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by __ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#' @param save_loc file path for saving data incl. extension ("html" or "csv") (default NULL)
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
get_series_exp <- function(exp_id, format = "wide", expand = "true", rename = "compact", descr = FALSE, save_loc = NULL) {
  if (is.null(Sys.getenv("udaman_token"))) stop("UDAMAN token is not available in .Renviron")
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

  # is a file requested?
  if (!is.null(save_loc)) {
    data_tbl %>% readr::write_csv(file = save_loc)
  }

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
#' @param start date of series start (character: "yyyy-mm-dd")
#' @param end date of series end (character: "yyyy-mm-dd")
#' @param per periodicity of series (character: "quarter", "year")
#' @param val values to fill in (numeric scalar or vector)
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
#' @param ser_in a variable name (character string with substituted expressions)
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
#' @param ser_in series names (character "mnemonic_loc", "mnemonic__loc_freq", mnemonic@loc.freq")
#' @param freq frequency of the series, required if not contained in the series
#'   name (character "D", "W", "M", "Q", "S", "A")
#'
#' @return series names following udaman convention "mnemonic@loc.freq"
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
  x_mod <- conv_long(x)
  in_list <- x_mod$long_form %>%
    tsbox::ts_tslist()

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
#' @examples
#' monthly_data_example |> copy_tbl()
copy_tbl <- function(x) {
  readr::write_delim(x, pipe("pbcopy"), delim = "\t")
}


# DON'T NEED THIS FUNCTION, ONLY NEED TO CHECK FOR WIDE
# #' Check if a data frame is in long format
# #'
# #' @param x tibble or data frame
# #'
# #' @return returns TRUE for long format data frame (id, time, value columns), FALSE otherwise
# #' @export
# #'
# #' @examples
# #' monthly_data_example |> is_long()
# #' monthly_data_example |> tsbox::ts_long() |> is_long()
# is_long <- function(x) {
#   rc <- tsbox::relevant_class(x)
#   if (!(rc %in% c("data.frame", "tbl_df", "data.table"))) {
#     return(FALSE)
#   }
#
#   x <- as.data.frame(x)
#
#   all.names <- colnames(x)
#   if (any(c("time", "times", "date", "dates") %in% tolower(colnames(x)))) {
#     time.name <- "time"
#   } else {
#     stop(
#       "No [time] column detected. ",
#       "To be explicit, name time column 'time'."
#     )
#   }
#
#   time.pos <- which(all.names == time.name)
#   id.names <- setdiff(all.names[1:time.pos], time.name)
#   value.names <- setdiff(all.names[time.pos:length(all.names)], time.name)
#
#   # character cols or factors should be considered ids, with message
#   value.classes <- vapply(x[, value.names], class, "")
#   value.names.that.are.ids <- names(value.classes)[value.classes %in% c("character", "factor")]
#
#   if (length(value.names.that.are.ids) > 0) {
#     # message(
#     #   "Found character or factor columns right to the [time] column.\n",
#     #   "These will be treated as [id] columns: ",
#     #   paste(paste0("'", value.names.that.are.ids, "'"), collapse = ", ")
#     # )
#     value.names <- setdiff(value.names, value.names.that.are.ids)
#     id.names <- union(id.names, value.names.that.are.ids)
#   }
#
#   if (length(value.names) == 0L) {
#     stop(
#       "No [value] column detected. ",
#       "[value] column(s) must be right of the [time] column."
#     )
#   }
#   if (length(value.names) > 1L) {
#     # message(
#     #   "Long tables should have a single [value] column.\n",
#     #   "The data contains the following [value] columns: ",
#     #   paste(paste0("'", value.names, "'"), collapse = ", ")
#     # )
#     return(FALSE)
#   } else if (length(id.names) == 0L) {
#     # message(
#     #   "Long tables should have a single [id] column. ",
#     #   "The data contains no [id] columns."
#     # )
#     return(FALSE)
#   } else if (length(id.names) > 1L) {
#     # message(
#     #   "Long tables should have a single [id] column.\n",
#     #   "The data contains the following [id] columns: ",
#     #   paste(paste0("'", id.names, "'"), collapse = ", ")
#     # )
#     return(FALSE)
#   } else {
#     return(TRUE)
#   }
# }


#' Check if a data frame is in wide format
#'
#' @param x tibble or data frame
#'
#' @return returns TRUE for wide format data frame (time and value columns), FALSE otherwise
#' @export
#'
#' @examples
#' monthly_data_example |> is_wide()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   is_wide()
#' dat_in <- monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_tslist()
#' wide_df <- is_wide(dat_in)
#' x_mod <- if (wide_df) tsbox::ts_long(dat_in) else tsbox::ts_tbl(dat_in)
#' ans <- if (wide_df) tsbox::ts_wide(x_mod) else tsbox::copy_class(x_mod, dat_in)
is_wide <- function(x) {
  rc <- tsbox::relevant_class(x)
  if (!(rc %in% c("data.frame", "tbl_df", "data.table"))) {
    return(FALSE)
  }

  x <- as.data.frame(x)

  all.names <- colnames(x)
  if (any(c("time", "times", "date", "dates") %in% tolower(colnames(x)))) {
    time.name <- "time"
  } else {
    stop(
      "No [time] column detected. ",
      "To be explicit, name time column 'time'."
    )
  }

  time.pos <- which(all.names == time.name)
  id.names <- setdiff(all.names[1:time.pos], time.name)
  value.names <- setdiff(all.names[time.pos:length(all.names)], time.name)

  # character cols or factors should be considered ids, with message
  value.classes <- vapply(x[, value.names], class, "")
  value.names.that.are.ids <- names(value.classes)[value.classes %in% c("character", "factor")]

  if (length(value.names.that.are.ids) > 0) {
    # message(
    #   "Found character or factor columns right to the [time] column.\n",
    #   "These will be treated as [id] columns: ",
    #   paste(paste0("'", value.names.that.are.ids, "'"), collapse = ", ")
    # )
    value.names <- setdiff(value.names, value.names.that.are.ids)
    id.names <- union(id.names, value.names.that.are.ids)
  }

  if (length(value.names) == 0L) {
    stop(
      "No [value] column detected. ",
      "[value] column(s) must be right of the [time] column."
    )
  }
  if (length(id.names) > 0L) {
    # message(
    #   "Wide tables should not have an [id] column.\n",
    #   "The data contains the following [id] columns: ",
    #   paste(paste0("'", id.names, "'"), collapse = ", ")
    # )
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Convert "ts-boxable" objects into long format
#'
#' @param x a "tx-boxable" object to be converted
#'
#' @return a list(long_form, was_wide, ser_names), where
#' x_mod is a ts-boxable object in long format with id, time and value columns,
#' was_wide is TRUE if x is a wide data frame, FALSE otherwise,
#' ser_names are the names of the series in x.
#' @export
#'
#' @details This function converts wide data frames and other ts-boxable objects
#' to the long format (wide data frames are not ts-boxable). In addition, it
#' ensures that objects containing a single time series have an id column.
#'
#' @examples
#' quarterly_data_example |>
#'   conv_long()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   conv_long()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   tsbox::ts_xts() |>
#'   conv_long()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   conv_long()
conv_long <- function(x) {
  # need xts series names for differential treatment of univariate data
  ser_names_1 <- names(x)
  # check if wide table
  wide_form <- is_wide(x)
  # convert to long table (all formats incl. xts)
  x_mod <-
    {
      if (wide_form) tsbox::ts_long(x) else tsbox::ts_tbl(x)
    } %>%
    tidyr::drop_na()
  # need long tbl series names for differential treatment of univariate data
  ser_names_2 <- tsbox::ts_summary(x_mod) %>%
    dplyr::pull(.data$id)
  ser_names <- if (length(ser_names_1) == 1) ser_names_1 else if (length(ser_names_2) == 1) ser_names_2 else ser_names_2
  # add an id column to univariate data
  x_mod <- x_mod %>%
    {
      if (length(ser_names) == 1) {
        dplyr::mutate(., id = ser_names, .before = "time")
      } else {
        .
      }
    } %>%
    tsbox::ts_default() %>%
    tsbox::ts_regular()

  return(list(long_form = x_mod, was_wide = wide_form, ser_names = ser_names))
}


# **************************
# time series utility functions ----
# **************************


#' Interpolate a single time series from low to high freqency
#'
#' @param x a single time series (e.g. xts) at low freq (e.g. annual or quarterly)
#' @param conv_type match the quarterly value via "first", "last", "sum", "mean"
#' @param target_freq target frequency "quarter" (default) or "month"
#'
#' @return time series at the target frequency
#'
#' @noRd
#' @examplesIf interactive()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   tsbox::ts_xts() |>
#'   disagg_1(conv_type = "mean", target_freq = "month") |>
#'   tsbox::ts_plot()
disagg_1 <- function(x, conv_type, target_freq) {
  tempdisagg::td(
    formula = x ~ 1,
    conversion = conv_type,
    to = target_freq,
    method = "fast"
  ) %>%
    stats::predict()
}


#' Interpolate univariate or multivariate time series from low to high frequency
#'
#' @param x a tx-boxable object at a low frequency (e.g. annual or quarterly)
#' @param conv_type match the quarterly value via "first", "last", "sum", "mean"
#' @param target_freq target frequency "quarter" or "month"
#'
#' @return interpolated object of the same type as the input
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   disagg(conv_type = "mean", target_freq = "month")
#' quarterly_data_example |>
#'   disagg(conv_type = "mean", target_freq = "month") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_frequency(to = "quarter", aggregate = "mean") |>
#'   tsbox::ts_wide() # this matches original data
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   disagg(conv_type = "mean", target_freq = "month") |>
#'   tsbox::ts_plot() # works with a single series too
disagg <- function(x, conv_type = "mean", target_freq = "quarter") {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  # drop missing values convert to xts and interpolate
  x_mod_int <- x_mod$long_form %>%
    tsbox::ts_xts() %>%
    tsbox::ts_apply(fun = disagg_1, conv_type = conv_type, target_freq = target_freq) %>%
    # univariate data requires special treatment
    {
      if (length(x_mod$ser_names) == 1) {
        tsbox::ts_tbl(.) %>%
          tsbox::ts_long() %>%
          dplyr::mutate(id = x_mod$ser_names)
      } else {
        tsbox::ts_tbl(.)
      }
    }

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_int) else tsbox::copy_class(x_mod_int, x)

  return(ans)
}


#' Linear interpolation based on AREMOS command reference page 292
#' (superseded by disagg())
#'
#' @param ser_in the xts series to be interpolated (freq = a)
#' @param aggr interpolation method: aggregate via mean (default) or sum
#'
#' @return interpolated xts series (freq = q)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   QtoA() |> # this matches with below
#'   AtoQ() |>
#'   QtoA() |> # this matches with above
#'   tsbox::ts_plot()
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


#' Aggregate from quarterly to annual frequency
#' (superseded by tsbox::ts_frequency())
#'
#' @param ser_in the xts series to be converted (freq = q)
#' @param	aggr aggregate via mean (default) or sum
#'
#' @return converted xts series (freq = a)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   QtoA() |> # this matches with below
#'   AtoQ() |>
#'   QtoA() |> # this matches with above
#'   tsbox::ts_plot()
QtoA <- function(ser_in, aggr = "mean") {
  ser_out <- tsbox::ts_frequency(ser_in, to = "year", aggregate = aggr)
  colnames(ser_out) <- colnames(ser_in) %>%
    gsub(".SOLQ", ".SOLA", .) %>%
    gsub(".Q", ".A", .)
  return(ser_out)
}


#' Year to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return year to date average, if FALSE, return year to date sum
#'
#' @return object of the same type as the input containing year to date sum or average
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   ytd_cum()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   ytd_cum(avg = FALSE) |>
#'   tsbox::ts_plot()
ytd_cum <- function(x, avg = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_ytd <- x_mod$long_form %>%
    dplyr::mutate(yr = lubridate::floor_date(.data$time, "year")) %>%
    dplyr::group_by(.data$id, .data$yr) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"yr")

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_ytd) else tsbox::copy_class(x_mod_ytd, x)

  return(ans)
}

#' Year to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing year to date growth rate
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   ytd_gr() |>
#'   tail()
ytd_gr <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_ytd_gr <- x_mod$long_form %>%
    ytd_cum() %>%
    tsbox::ts_pcy()

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_ytd_gr) else tsbox::copy_class(x_mod_ytd_gr, x)

  return(ans)
}


#' Month to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return month to date average, if FALSE, return month to date sum
#'
#' @return object of the same type as the input containing year to date sum or average
#' @export
#'
#' @examples
#' daily_data_example |>
#'   mtd_cum()
#' test <- daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   mtd_cum()
#' tsbox::`%ts/%`(test, tsbox::ts_lag(test, "6 months")) |> tail()
mtd_cum <- function(x, avg = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_mtd <- x_mod$long_form %>%
    dplyr::mutate(yrmo = lubridate::floor_date(.data$time, "month")) %>%
    dplyr::group_by(.data$id, .data$yrmo) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"yrmo")

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_mtd) else tsbox::copy_class(x_mod_mtd, x)

  return(ans)
}


#' Month to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing month to date growth rate
#' @export
#'
#' @examples
#' daily_data_example |>
#'   mtd_gr() |>
#'   tail()
mtd_gr <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_mtd_gr <- x_mod$long_form %>%
    mtd_cum() %>%
    tsbox::ts_pcy()

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_mtd_gr) else tsbox::copy_class(x_mod_mtd_gr, x)

  return(ans)
}


#' Period to date sum or average
#'
#' @param x a ts-boxable object
#' @param per unit of time supplied to floor_date() (for ytd per = "year"
#'   (default), for mtd per = "month")
#' @param avg if TRUE (default), retorn period to date average, if FALSE, return period to date sum
#'
#' @return object of the same type as the input containing period to date sum or average
#' @export
#'
#' @examples
#' daily_data_example |>
#'   ptd_cum("week")
#' test <- daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   ptd_cum("week")
#' tsbox::`%ts/%`(test, tsbox::ts_lag(test, "4 weeks")) |>
#'   tsbox::`%ts-%`(1) |>
#'   tsbox::`%ts*%`(100) |>
#'   tail()
ptd_cum <- function(x, per = "year", avg = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_ptd <- x_mod$long_form %>%
    dplyr::mutate(time_per = lubridate::floor_date(.data$time, per)) %>%
    dplyr::group_by(.data$id, .data$time_per) %>%
    dplyr::mutate(value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"time_per")

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_ptd) else tsbox::copy_class(x_mod_ptd, x)

  return(ans)
}


#' Period to date growth rate
#'
#' @param x a ts-boxable object
#' @param per unit of time supplied to floor_date() (for ytd per = "year"
#'   (default), for mtd per = "month")
#' @param lag_length period over which growth is calculated (e.g. "1 year"
#'   (default), "3 years", etc. See ?ts_lag() for options)
#'
#' @return object of the same type as the input containing period to date growth rate
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   ptd_gr() |>
#'   tail()
#' monthly_data_example |>
#'   dplyr::select(time, "VAPNS_HI") |>
#'   ptd_gr(per = "month", lag_length = "3 years") |>
#'   tail()
#' daily_data_example |>
#'   ptd_gr("week")
#' daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   ptd_gr("week", "4 weeks") %>%
#'   tail()
ptd_gr <- function(x, per = "year", lag_length = "1 year") {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_ptd_gr <- x_mod$long_form %>%
    ptd_cum(per) %>%
    {
      (. %ts/% tsbox::ts_lag(., lag_length) %ts-% 1) %ts*% 100
    }

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_ptd_gr) else tsbox::copy_class(x_mod_ptd_gr, x)

  return(ans)
}


#' Convert annualized growth to quarterly growth
#'
#' @param x ts-boxable object containing annualized growth (in percent)
#' @param freq numeric frequency of the time series e.g. 4 for quarterly
#'
#' @return object of the same type as the input containing quarterly growth (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pca() |>
#'   pca_to_pc() |>
#'   tail()
#' tsbox::ts_c(
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pca() |>
#'     pca_to_pc(),
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pc()
#' ) |>
#'   dplyr::arrange(id, time) |>
#'   tsbox::ts_wide()
pca_to_pc <- function(x, freq = 4) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_pc <- x_mod$long_form %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^(1 / freq) - 1) * 100)

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_pc) else tsbox::copy_class(x_mod_pc, x)

  return(ans)
}


#' Convert quarterly growth to annualized growth
#'
#' @param x ts-boxable object containing quarterly growth (in percent)
#' @param freq numeric frequency of the time series e.g. 4 for quarterly
#'
#' @return object of the same type as the input containing annualized growth (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pc() |>
#'   pc_to_pca() |>
#'   tail()
#' tsbox::ts_c(
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pc() |>
#'     pc_to_pca(),
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pca()
#' ) |>
#'   dplyr::arrange(id, time) |>
#'   tsbox::ts_wide()
pc_to_pca <- function(x, freq = 4) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_pc <- x_mod$long_form %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^freq - 1) * 100)

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_pc) else tsbox::copy_class(x_mod_pc, x)

  return(ans)
}


#' Calculate multi-period average growth
#'
#' @param x ts-boxable object for which growth is calculated (in levels)
#' @param lag number of period over which growth is calculated
#'
#' @return object of the same type as the input ontaining the average growth of x (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   pcmp(20) |>
#'   tail()
pcmp <- function(x, lag = 4) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_pcmp <- x_mod$long_form %>%
    tsbox::ts_tslist() %>%
    purrr::map(~ (((.x / tsbox::ts_lag(.x, lag))^(1 / lag)) - 1) * 100) %>%
    magrittr::set_attr("class", c("list", "tslist")) %>%
    tsbox::ts_tbl()

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_pcmp) else tsbox::copy_class(x_mod_pcmp, x)

  return(ans)
}


#' Find the date of the first observation (NAs are dropped)
#'
#' @param x ts-boxable object
#'
#' @return dates associated with first observation
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   dplyr::mutate(E_NF_HI = dplyr::if_else(time < "2000-01-01", NA_real_, E_NF_HI)) |>
#'   find_start()
find_start <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod$long_form %>%
    tsbox::ts_summary() %>%
    dplyr::pull(.data$start)
}


#' Find the date of the last observation (NAs are dropped)
#'
#' @param x ts-boxable object
#'
#' @return date associated with last observation
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   dplyr::mutate(E_NF_HI = dplyr::if_else(time > "2022-01-01", NA_real_, E_NF_HI)) |>
#'   find_end()
find_end <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod$long_form %>%
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


# #' DON'T USE!!! USE lubridate::yq() INSTEAD
# #' Concatenate date formatted as yyyyQq or yyyy.q to obtain single period
# #'
# #' @param dat1 date of period start (string: yyyyQq or yyyy.q)
# #'
# #' @return string containing date range
# #' @export
# #'
# #' @examples
# #' pq_1("2010Q1")
# #' pq_1(2010.1)
# pq_1 <- function(dat1 = "") {
#   if (dat1 != "") dat1 %>% lubridate::yq()
# }


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
#' @param x ts-boxable object
#' @param order numeric order (window length) of moving average, includes contemporaneous observation
#'
#' @return object of the same type as the input containing moving average
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   ma(4) |>
#'   head()
ma <- function(x, order) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  x_mod_ma <- x_mod$long_form %>%
    dplyr::mutate(value = slider::slide_dbl(.data$value, mean, .before = order - 1, .complete = TRUE), .by = "id")

  # reclass the output to match the input
  ans <- if (x_mod$was_wide) tsbox::ts_wide(x_mod_ma) else tsbox::copy_class(x_mod_ma, x)

  return(ans)
}


# **************************
# plotting functions ----
# **************************
#' Interactive plot with level and growth rate
#'
#' @param x ts-boxable object to plot (e.g. time series of history, oldsol, sol)
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
#' monthly_data_example |>
#'   plot_1()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_1()
plot_1 <- function(x, rng_start = as.character(Sys.Date() - lubridate::years(10)), rng_end = as.character(Sys.Date() + lubridate::years(2)), height = 300, width = 900, yoy_gr = TRUE, gr_1 = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  ser_names <- x_mod$ser_names

  ser_names_pct <- if (gr_1) stringr::str_glue("{ser_names}%")[1] else stringr::str_glue("{ser_names}%")

  ser_plot <- x_mod$long_form %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_c(., tsbox::ts_pcy(.)) else tsbox::ts_c(., tsbox::ts_pc(.))
    } %>%
    # magrittr::extract(, 1:length(c(ser_names, ser_names_pct))) %>%
    tsbox::ts_pick(1:length(c(ser_names, ser_names_pct))) %>%
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
#' @param x ts-boxable object to plot (e.g. time series of history, oldsol, sol)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#'
#' @return a dygraph plot
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_2ax()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_2ax()
plot_2ax <- function(x, rng_start = as.character(Sys.Date() - lubridate::years(10)), rng_end = as.character(Sys.Date() + lubridate::years(2)), height = 300, width = 900) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  ser_names <- x_mod$ser_names

  ser_plot <- x_mod$long_form %>%
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
#' @param x ts-boxable object to plot (min 1, max 3 time series) (e.g. current fcst, old fcst, history)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param add_table should a data table be appended to the plot? (default = TRUE)
#' @param table_start start of table range ("YYYY-MM-DD") (all data = NULL, default = rng_start)
#' @param table_end end of table range ("YYYY-MM-DD") (all data = NULL, default = rng_end)
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#'
#' @return a dygraph plot
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_fc()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_fc()
plot_fc <- function(x, rng_start = as.character(Sys.Date() - lubridate::years(10)), rng_end = as.character(Sys.Date() + lubridate::years(2)), add_table = TRUE, table_start = rng_start, table_end = rng_end, height = 300, width = 900, yoy_gr = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  ser_names <- x_mod$ser_names

  # series to plot
  ser_to_plot <- x_mod$long_form %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_c(., tsbox::ts_pcy(.[, 1])) else tsbox::ts_c(., tsbox::ts_pc(.[, 1]))
    } %>%
    magrittr::set_names(c(ser_names, stringr::str_glue("{ser_names[1]}%")))

  # generate the plot
  ser_plot <- ser_to_plot %>%
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

  if (add_table) {
    # generate a table with the data
    ser_tbl <- ser_to_plot %>%
      tsbox::ts_tbl() %>%
      tidyr::drop_na() %>%
      tsbox::ts_span(table_start, table_end) %>%
      dplyr::mutate(value = round(.data$value, 2)) %>%
      tsbox::ts_wide() %>%
      reactable::reactable(
        highlight = TRUE,
        compact = TRUE,
        height = height,
        width = width,
        defaultPageSize = 1000
      )
    ser_plot <- list(ser_plot, ser_tbl)
  }

  # render the dygraphs objects and tables using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Two-panel plot of levels and growth rates
#'
#' @param x ts-boxable object to plot
#' @param rng_start start of the zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr year-over-year (default) or annualized growth
#' @param gr_bar show bars or line (default) for the growth series
#'
#' @return a list with two dygraph plots (level, growth)
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_comp_2()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_comp_2()
plot_comp_2 <- function(x, rng_start = as.character(Sys.Date() - lubridate::years(10)), rng_end = as.character(Sys.Date() + lubridate::years(2)), height = 300, width = 900, yoy_gr = TRUE, gr_bar = FALSE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  ser_names <- x_mod$ser_names

  plot_level <-
    x_mod$long_form %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(main = "Level", group = "comp", height = height, width = width) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    x_mod$long_form %>%
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
  list(plot_level, plot_growth) %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Three-panel plot of levels, index, and growth rates
#'
#' @param x ts-boxable object to plot
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
#' monthly_data_example |>
#'   plot_comp_3()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_comp_3()
plot_comp_3 <- function(x, indx_start = as.character(Sys.Date() - lubridate::years(10)), rng_start = as.character(Sys.Date() - lubridate::years(10)), rng_end = as.character(Sys.Date() + lubridate::years(2)), height = 300, width = 900, yoy_gr = TRUE, gr_bar = FALSE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  ser_names <- x_mod$ser_names

  plot_level <-
    x_mod$long_form %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(main = "Level", group = "comp", height = height, width = width) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  # plot_level[["elementId"]] <- ser_names %>% extract(1) %>% str_extract("^.*@")
  plot_index <-
    x_mod$long_form %>%
    tsbox::ts_xts() %>%
    tsbox::ts_index(base = indx_start) %>%
    tsbox::ts_dygraphs(main = "Index", group = "comp", height = height, width = width) %>%
    # dygraphs::dyRebase(value = 100) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uhero_colors[1:length(ser_names)]) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    x_mod$long_form %>%
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


#' Save a list of interactive plots to html
#'
#' @param plot_list a list of plots generated by fcutils::plot_xxxx() functions
#' @param save_loc location to save the plots to, including file name
#'
#' @return nothing (silently save the html to a user defined location)
#' @export
#'
#' @examples
#' # hold the plots in a list
#' plot_out <- list()
#' for (i in monthly_data_example[2:3] |> names()) {
#'   plot_out[[i]] <- plot_1(
#'     monthly_data_example |> tsbox::ts_long() |>
#'       dplyr::filter(stringr::str_detect(id, i)),
#'     rng_start = as.character(Sys.Date() - lubridate::years(5)),
#'     rng_end = as.character(Sys.Date() + lubridate::years(7)),
#'     width = 1500, height = 650, yoy_gr = TRUE
#'   )
#' }
#' # specify location of the output
#' save_loc <- stringr::str_c("~/Downloads/plots_", Sys.Date(), ".html")
#' # combine a list of plots into a single html
#' @examplesIf interactive()
#' plot_out |> save_plot_list(save_loc)
save_plot_list <- function(plot_list, save_loc) {
  # save list of plots to html
  plot_list %>%
    htmltools::tagList() %>%
    htmltools::browsable() %>%
    htmltools::save_html(file = save_loc)
  # fix the html
  readr::read_lines(file = save_loc)[-1] %>%
    stringr::str_replace_all(c("<style>" = "\\\n<title>Plots</title>\\\n<style>", "\\}</style>" = "font-family:Arial,Helvetica,sans-serif;font-size:medium;}</style>")) %>%
    readr::write_lines(file = save_loc)
  # incorporate dependecies into html
  rmarkdown::pandoc_self_contained_html(input = save_loc, output = save_loc)
  # check out htmlwidgets
}


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
gen_table <- function(x, tbl_start = as.character(Sys.Date() - lubridate::years(10)), tbl_end = as.character(Sys.Date() + lubridate::years(2)), percent = "pc", time_across = TRUE, tbl_height = 800, save_loc = NULL) {
  # convert to long format and return additional details
  x_mod <- conv_long(x)

  # add growth rates and format table for output
  tbl_out <- x_mod$long_form %>%
    {
      if (percent == "pc") {
        tsbox::ts_c(., tsbox::ts_pc(.) %>% dplyr::mutate(id = stringr::str_c(.data$id, " (%)")))
      } else if (percent == "pcy") {
        tsbox::ts_c(., tsbox::ts_pcy(.) %>% dplyr::mutate(id = stringr::str_c(.data$id, " (YoY%)")))
      } else if (percent == "pca") {
        tsbox::ts_c(., tsbox::ts_pca(.) %>% dplyr::mutate(id = stringr::str_c(.data$id, " (Ann%)")))
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

          searchMethod = htmlwidgets::JS("function(rows, columnIds, searchValue) {
        const pattern = new RegExp(searchValue, 'i')
        return rows.filter(function(row) {
        return columnIds.some(function(columnId) {
        return pattern.test(row.values[columnId])
        })
        })
        }"),
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
          defaultColDef = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 2))
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
          defaultColDef = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 2))
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
