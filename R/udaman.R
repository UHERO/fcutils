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
#'   and no frequency; "full": @ replaced by _AT_ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#' @param public if TRUE use the public API interface - does not require VPN (default: FALSE)
#'
#' @return time and data for a single series combined in a tibble
#'
#' @details This function requires permission to access UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890="
#' Or run set_udaman_token(key) to set it for the current session only.
#' Or store the udaman token among your credentials (e.g. keychain) using keyring:
#' keyring::key_set_with_value(service = "udaman_token", password = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
#'
#' @noRd
#' @examplesIf interactive()
#' get_series_1(ser_id = "VISNS@HI.M")
get_series_1 <- function(ser_id, expand, rename, descr, public) {
  uhero_data <- udaman_get(
    stringr::str_c("series?name=", ser_id, "&expand=", expand),
    public
  )
  # extract series info
  dates <- uhero_data$data$observations$transformationResults$dates[[1]]
  values <- uhero_data$data$observations$transformationResults$values[[1]]
  series <- dplyr::bind_cols(
    time = lubridate::ymd(dates),
    values = as.numeric(values)
  )
  name <- uhero_data$data$series$name
  if (name == "") {
    series <- dplyr::bind_cols(
      time = seq(
        lubridate::ymd("2000-01-01"),
        lubridate::ymd("2010-01-01"),
        by = "year"
      ),
      values = rep(NA_real_, 11)
    )
    name <- ser_id
  }
  cat(name, "\n")
  series <- tsbox::ts_regular(series)
  title <- uhero_data$data$series$title
  geo <- uhero_data$data$series$geography$shortName
  colnames(series) <- c(
    "time",
    udaman_names(name, rename, descr, stringr::str_c(title, ", ", geo))
  )
  # return series
  return(series)
}


#' Download a set of series from udaman using series names
#'
#' @param ser_id_vec vector of series names (character)
#' @param format "wide" (default) or "long" or "xts"
#' @param raw TRUE (default) or FALSE (TRUE downloads raw data, FALSE downloads scaled and rounded data)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by _AT_ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param freq if frequency is missing from series names (or want to modify freq
#'   in existing names) specify frequency (character), e.g. "M".
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#' @param public if TRUE use the public API interface - does not require VPN (default: FALSE)
#'
#' @return time and data for all series combined in an object specified by the
#'   format option
#' @export
#'
#' @details This function requires permission to access UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890="
#' Or run set_udaman_token(key) to set it for the current session only.
#' Or store the udaman token among your credentials (e.g. keychain) using keyring:
#' keyring::key_set_with_value(service = "udaman_token", password = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
#'
#' @examplesIf interactive()
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M"), raw = TRUE)
#' get_series(c("VEXP_RB@HI.M"))
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M"), public = TRUE)
#' get_series(c("VISNS@HI.M", "VISUSNS@HI.M"), freq = "Q")
#' get_series(c("VISNS@HI.M", "VAPNS@HI.M"), format = "xts")
#' get_series(c("VISNS@HI.M"), format = "xts")
#' get_series(c("VISNS@HI.M"), format = "xts", descr = TRUE)
#' get_series(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#' get_series(c("E_NF__HI_M", "ECT__HI_M", "VAP__HI_W"))
#' get_series(c("E_NF_AT_HI_M", "ECT_AT_HI_M", "VAP_AT_HI_W"))
#' get_series("E_NF_HI5_M , ECT__HIALL_Q  E_TU@CNTY.A", rename = "no")
#' get_series(c("E_NF_HI5 , ECT__HIALL  E_TU@CNTY", "VAP_HAW ; ECT_HON"), freq = "M")
get_series <- function(
  ser_id_vec,
  format = "wide",
  raw = TRUE,
  rename = "compact",
  freq = NULL,
  descr = FALSE,
  public = FALSE
) {
  expand <- if (raw) "raw" else "true"
  ser_tbl <- ser_id_vec %>%
    rename_udaman(., freq = freq) %>%
    purrr::map(
      get_series_1,
      expand = expand,
      rename = rename,
      descr = descr,
      public = public
    ) %>%
    purrr::reduce(dplyr::full_join, by = "time") %>%
    dplyr::arrange(.data$time)
  return(udaman_format(ser_tbl, format))
}


#' Download series listed in an export table from udaman
#'
#' @param exp_id export id (character or numeric)
#' @param format "wide" (default) or "long" or "xts"
#' @param raw TRUE (default) or FALSE (TRUE downloads raw data, FALSE downloads scaled and rounded data)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by _AT_ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#' @param public if TRUE use the public API interface - does not require VPN (default: FALSE)
#' @param save_loc path of a CSV file to write (default NULL: no file). The
#'   file always holds the wide table, whatever `format` is.
#'
#' @return the series in the format chosen by `format`
#' @export
#'
#' @details This function requires permission to access UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890="
#' Or run set_udaman_token(key) to set it for the current session only.
#' Or store the udaman token among your credentials (e.g. keychain) using keyring:
#' keyring::key_set_with_value(service = "udaman_token", password = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
#'
#' @examplesIf interactive()
#' get_series_exp(exp_id = 74)
#' get_series_exp(74, format = "xts")
get_series_exp <- function(
  exp_id,
  format = "wide",
  raw = TRUE,
  rename = "compact",
  descr = FALSE,
  public = FALSE,
  save_loc = NULL
) {
  expand <- if (raw) "raw" else "true"
  uhero_data <- udaman_get(
    stringr::str_c("package/export?id=", exp_id, "&expand=", expand),
    public
  )
  # extract series info
  dates <- uhero_data$data$seriesObservations$transformationResults %>%
    purrr::map("dates") %>%
    purrr::flatten()
  values <- uhero_data$data$seriesObservations$transformationResults %>%
    purrr::map("values") %>%
    purrr::flatten()
  data_lst <- purrr::map2(
    dates,
    values,
    ~ tibble::tibble(
      time = .x %>% lubridate::ymd(),
      value = .y %>% as.numeric()
    )
  )
  empty_ser <- data_lst %>%
    purrr::map(~ nrow(.x) == 0) %>%
    purrr::reduce(c)
  cat(stringr::str_c(
    "The following series did not contain data: ",
    uhero_data$data$name[empty_ser],
    collapse = "\n"
  ))
  # one long table, then wide: merging the series one pair at a time with
  # ts_c() takes time that grows with the square of the number of series
  # data_tbl <- data_lst %>%
  #   purrr::discard(~ nrow(.x) == 0) %>%
  #   purrr::reduce(tsbox::ts_c) %>%
  #   tsbox::ts_wide()
  data_lst <- purrr::discard(data_lst, ~ nrow(.x) == 0)
  names(data_lst) <- sprintf("s%06d", seq_along(data_lst))
  data_tbl <- dplyr::bind_rows(data_lst, .id = "id") %>%
    tsbox::ts_wide()
  name <- uhero_data$data$name[!empty_ser]
  title <- uhero_data$data$title[!empty_ser]
  # geo <- NULL
  # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
  colnames(data_tbl) <- c("time", udaman_names(name, rename, descr, title))

  # is a file requested?
  if (!is.null(save_loc)) {
    data_tbl %>% readr::write_csv(file = save_loc)
  }

  return(udaman_format(data_tbl, format))
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
#' @param ser_in a name with glue expressions in braces, such as
#'   `"E{ser_i}_{cnty_i}"`. The expressions are evaluated in the global
#'   environment.
#' @param env environment in which the resulting name is looked up
#'
#' @return the object with that name
#' @export
#'
#' @examples
#' ser_i <- "_NF"
#' cnty_i <- "HI"
#' q <- conv_xts(quarterly_data_example)
#' with(as.list(q), get_var("E{ser_i}_{cnty_i}")) |> tail(2)
get_var <- function(ser_in, env = parent.frame()) {
  return(ser_in %>% stringr::str_glue() %>% get(envir = env, inherits = TRUE))
}


#' Format series names to udaman format (mnemonic@loc.freq)
#'
#' @param ser_in series names in any of the forms MNEMONIC_LOC,
#'   MNEMONIC__LOC_F, MNEMONIC_AT_LOC_F or MNEMONIC@LOC.F. One string may
#'   hold several names separated by commas, semicolons or spaces.
#' @param freq what follows the location, replacing any frequency in the
#'   names: a frequency ("D", "W", "M", "Q", "S", "A"), another suffix such
#'   as "SOLA" for an AREMOS solution bank, or "" for none (`UR@US`). NULL
#'   (default) keeps the frequency in the names, which then must have one.
#'
#' @return series names following udaman convention "mnemonic@loc.freq"
#' @export
#'
#' @details In MNEMONIC_AT_LOC and MNEMONIC@LOC the location is the part
#' after `_AT_` or `@`, so any location works (CAN, CA, ...). In MNEMONIC_LOC
#' and MNEMONIC__LOC the location must be one of HI, HON, HAW, MAU, KAU,
#' MAUI, MOL, LAN, NBI, US and JP, just before the frequency or the end of
#' the name. HI5, HI3, HIALL and CNTY expand to the matching sets of
#' counties, so the output can be longer than the input. A name whose
#' location cannot be read, or with no frequency when `freq` is NULL, stops
#' with an error naming it.
#'
#' @examples
#' rename_udaman(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#' rename_udaman(c("E_NF__HI_M", "ECT__HI_M", "VAP__HAW_W"))
#' rename_udaman(c("E_NF_AT_HI_M", "ECT_AT_HI_M", "VAP_AT_HAW_W"))
#' rename_udaman(c("E_NF@HI.M", "ECT@HI.M", "VAP@HAW.W"))
#' rename_udaman(c("SH_US@HI.M", "SH_JP__HON_M"))
#' rename_udaman(c("EAF_AT_HAW", "GDP_R_AT_CAN"), freq = "SOLA")
#' rename_udaman(c("UR_AT_US", "GDP_R_AT_US"), freq = "")
#' rename_udaman(c("E_NF_HI5", "ECT__HIALL", "E_TU@CNTY"), freq = "M")
#' rename_udaman(c("E_NF_HI5 , ECT__HIALL  E_TU@CNTY", "VAP_HAW ; ECT_HON"), freq = "M")
#' quarterly_data_example |>
#'   dplyr::rename_with(~ rename_udaman(., freq = "Q"), .cols = -1) |>
#'   names() |>
#'   head()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(id = rename_udaman(id, freq = "Q")) |>
#'   tsbox::ts_xts() |>
#'   colnames() |>
#'   head()
rename_udaman <- function(ser_in, freq = NULL) {
  if (!is.character(ser_in)) {
    ser_in <- as.character(ser_in)
  }
  if (stringr::str_detect(ser_in, "[,;\\s]") %>% any()) {
    ser_in <- stringr::str_split(ser_in, "[,;\\s]+") %>%
      purrr::list_c()
  }
  locs <- "HI5|HI3|HIALL|HI|CNTY|HON|HAW|MAUI|MOL|LAN|MAU|KAU|NBI|US|JP"
  # MNEMONIC_AT_LOC_F and MNEMONIC@LOC.F name the location, so any location
  # works; the suffix after it can be longer than one letter (SOLA)
  explicit <- stringr::str_match(
    ser_in,
    "^([[:alnum:]_]+?)(?:_AT_|@)([[:alnum:]]+)(?:[_.]([[:alpha:]]+))?$"
  )
  # MNEMONIC_LOC_F and MNEMONIC__LOC_F: a known location just before the
  # frequency (or the end), so a mnemonic that contains a location code, such
  # as SH_JP, is not mistaken for it
  compact <- stringr::str_match(
    ser_in,
    stringr::str_c("^([[:alnum:]_]+?)_+(", locs, ")(?:_([DWMQSA]))?$")
  )
  parts <- explicit
  parts[is.na(explicit[, 1]), ] <- compact[is.na(explicit[, 1]), ]
  if (anyNA(parts[, 1])) {
    rlang::abort(c(
      "rename_udaman(): cannot read the location in these names:",
      rlang::set_names(ser_in[is.na(parts[, 1])], "x")
    ))
  }
  suffix <- if (is.null(freq)) parts[, 4] else rep(freq, length(ser_in))
  if (anyNA(suffix)) {
    rlang::abort(c(
      "rename_udaman(): no frequency in these names; set `freq`:",
      rlang::set_names(ser_in[is.na(suffix)], "x")
    ))
  }
  suffix <- dplyr::if_else(suffix == "", "", stringr::str_c(".", suffix))

  # expand location abbreviations
  expand_loc <- function(loc) {
    switch(
      loc,
      HI5 = c("HI", "HON", "HAW", "MAU", "KAU"),
      HI3 = c("HI", "HON", "NBI"),
      HIALL = c("HI", "HON", "HAW", "MAU", "KAU", "MAUI", "MOL", "LAN"),
      CNTY = c("HON", "HAW", "MAU", "KAU"),
      loc
    )
  }

  purrr::pmap(
    list(parts[, 2], parts[, 3], suffix),
    function(mnemonic, loc, suffix) {
      stringr::str_c(mnemonic, "@", expand_loc(loc), suffix)
    }
  ) %>%
    purrr::list_c()
}


#' Set udaman token for API access
#'
#' @param key a string containing 44 characters
#'
#' @return NULL, invisibly
#' @export
#'
#' @details Sets the udaman token for this R session (environment variable
#' `udaman_token`) and warns if the key is not 44 characters long. To keep
#' the token for later sessions, add `udaman_token = "<key>"` to .Renviron,
#' or run `set_api_key("udaman", key, store = "keyring")`.
#'
#' @examplesIf interactive()
#' set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdef=")
set_udaman_token <- function(key) {
  if (!rlang::is_string(key)) {
    rlang::abort("`key` must be a string.")
  }
  if (nchar(key) != 44) {
    rlang::warn("`key` does not have a length of 44 characters.")
  }
  set_api_key("udaman", key)
}


#' Request from the udaman API and parse the JSON answer
#'
#' @param query path and query after the API version, e.g.
#'   "series?name=VISNS@HI.M&expand=raw"
#' @param public TRUE for the public API (no VPN needed)
#' @noRd
udaman_get <- function(query, public) {
  token <- api_key("udaman")
  base <- if (public) {
    "https://api.uhero.hawaii.edu/v1/"
  } else {
    "https://api.uhero.hawaii.edu/v1.u/"
  }
  req <- httr::GET(
    stringr::str_c(base, query, "&u=uhero&nocache"),
    httr::add_headers(Authorization = stringr::str_c("Bearer ", token))
  )
  json <- httr::content(req, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(json)
}


#' Column names for udaman series
#'
#' @param name udaman names (VISNS@HI.M)
#' @param rename "compact" (VISNS_HI), "full" (VISNS_AT_HI_M) or "no"
#' @param descr if TRUE, append " (label)"
#' @param label description appended when descr is TRUE
#' @noRd
udaman_names <- function(name, rename, descr, label) {
  out <- switch(
    rename,
    compact = stringr::str_replace_all(
      name,
      c("\\.[A-Z]" = "", "@" = "_", "OCUP%" = "OCUPP")
    ),
    full = stringr::str_replace_all(
      name,
      c("\\.([A-Z])" = "_\\1", "@" = "_AT_", "OCUP%" = "OCUPP")
    ),
    no = name,
    rlang::abort("`rename` must be \"compact\", \"full\" or \"no\".")
  )
  if (descr) stringr::str_c(out, " (", label, ")") else out
}


#' A wide udaman table in the requested format
#' @noRd
udaman_format <- function(tbl, format) {
  switch(
    format,
    wide = tbl,
    long = tsbox::ts_long(tbl),
    xts = tsbox::ts_xts(tsbox::ts_long(tbl)),
    rlang::abort("`format` must be \"wide\", \"long\" or \"xts\".")
  )
}
