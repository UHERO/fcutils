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
#' Or using fcutils::set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
#' Or store the udaman token among your credentials (e.g. keychain) using keyring:
#' keyring::key_set_with_value(service = "udaman_token", password = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
#'
#' @noRd
#' @examplesIf interactive()
#' get_series_1(ser_id = "VISNS@HI.M")
get_series_1 <- function(ser_id, expand, rename, descr, public) {
  if (
    stringr::str_length(Sys.getenv("udaman_token")) == 0 &
      length(keyring::key_list("udaman_token")$service) == 0
  ) {
    rlang::abort(
      "UDAMAN token is not available in .Renviron or among credentials"
    )
  }
  # API call
  if (public) {
    url <- stringr::str_c(
      "https://api.uhero.hawaii.edu/v1/series?name=",
      ser_id,
      "&expand=",
      expand,
      "&u=uhero&nocache"
    )
  } else {
    url <- stringr::str_c(
      "https://api.uhero.hawaii.edu/v1.u/series?name=",
      ser_id,
      "&expand=",
      expand,
      "&u=uhero&nocache"
    )
  }
  if (stringr::str_length(Sys.getenv("udaman_token")) > 0) {
    req <- httr::GET(
      url,
      httr::add_headers(
        Authorization = stringr::str_c("Bearer ", Sys.getenv("udaman_token"))
      )
    )
  } else {
    req <- httr::GET(
      url,
      httr::add_headers(
        Authorization = stringr::str_c(
          "Bearer ",
          keyring::key_get("udaman_token")
        )
      )
    )
  }
  json <- httr::content(req, as = "text", encoding = "UTF-8")
  uhero_data <- jsonlite::fromJSON(json)
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
  colnames(series) <- dplyr::case_when(
    rename == "compact" ~
      c(
        "time",
        name %>%
          stringr::str_replace_all(c(
            "\\.[A-Z]" = "",
            "@" = "_",
            "OCUP%" = "OCUPP"
          )) %>%
          {
            if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
          }
      ),
    rename == "full" ~
      c(
        "time",
        name %>%
          stringr::str_replace_all(c(
            "\\.([A-Z])" = "_\\1",
            "@" = "_AT_",
            "OCUP%" = "OCUPP"
          )) %>%
          {
            if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
          }
      ),
    rename == "no" ~
      c(
        "time",
        name %>%
          {
            if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
          }
      )
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
#' Or using fcutils::set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
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
  if (format == "wide") {
    ser_out <- ser_tbl
  }
  if (format == "long") {
    ser_out <- ser_tbl %>% tsbox::ts_long()
  }
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
#' @param raw TRUE (default) or FALSE (TRUE downloads raw data, FALSE downloads scaled and rounded data)
#' @param rename "compact" (default), "full", "no". "compact": @ replaced by _
#'   and no frequency; "full": @ replaced by _AT_ and . by _; "no": no renaming,
#'   keep UDAMAN names
#' @param descr if TRUE add to the udaman series name the series description in parentheses (default: FALSE)
#' @param public if TRUE use the public API interface - does not require VPN (default: FALSE)
#' @param save_loc file path for saving data incl. extension ("html" or "csv") (default NULL)
#'
#' @return time and data for all series combined in a tibble
#' @export
#'
#' @details This function requires permission to access UDAMAN.
#' Store the udaman token in the .Renviron file using the following format:
#' udaman_token = "-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890="
#' Or using fcutils::set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
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
  if (
    stringr::str_length(Sys.getenv("udaman_token")) == 0 &
      length(keyring::key_list("udaman_token")$service) == 0
  ) {
    rlang::abort(
      "UDAMAN token is not available in .Renviron or among credentials"
    )
  }
  # API call
  if (public) {
    url <- stringr::str_c(
      "https://api.uhero.hawaii.edu/v1/package/export?id=",
      exp_id,
      "&expand=",
      expand,
      "&u=uhero&nocache"
    )
  } else {
    url <- stringr::str_c(
      "https://api.uhero.hawaii.edu/v1.u/package/export?id=",
      exp_id,
      "&expand=",
      expand,
      "&u=uhero&nocache"
    )
  }
  if (stringr::str_length(Sys.getenv("udaman_token")) > 0) {
    req <- httr::GET(
      url,
      httr::add_headers(
        Authorization = stringr::str_c("Bearer ", Sys.getenv("udaman_token"))
      )
    )
  } else {
    req <- httr::GET(
      url,
      httr::add_headers(
        Authorization = stringr::str_c(
          "Bearer ",
          keyring::key_get("udaman_token")
        )
      )
    )
  }
  json <- httr::content(req, as = "text", encoding = "UTF-8")
  uhero_data <- jsonlite::fromJSON(json)
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
  data_tbl <- data_lst %>%
    purrr::discard(~ nrow(.x) == 0) %>%
    purrr::reduce(tsbox::ts_c) %>%
    tsbox::ts_wide()
  name <- uhero_data$data$name[!empty_ser]
  title <- uhero_data$data$title[!empty_ser]
  # geo <- NULL
  colnames(data_tbl) <- dplyr::case_when(
    rename == "compact" ~
      c(
        "time",
        name %>%
          stringr::str_replace_all(c(
            "\\.[A-Z]" = "",
            "@" = "_",
            "OCUP%" = "OCUPP"
          )) %>%
          {
            # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
            if (descr) stringr::str_c(., " (", title, ")") else .
          }
      ),
    rename == "full" ~
      c(
        "time",
        name %>%
          stringr::str_replace_all(c(
            "\\.([A-Z])" = "_\\1",
            "@" = "_AT_",
            "OCUP%" = "OCUPP"
          )) %>%
          {
            # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
            if (descr) stringr::str_c(., " (", title, ")") else .
          }
      ),
    rename == "no" ~
      c(
        "time",
        name %>%
          {
            # if (descr) stringr::str_c(., " (", title, ", ", geo, ")") else .
            if (descr) stringr::str_c(., " (", title, ")") else .
          }
      )
  )

  # is a file requested?
  if (!is.null(save_loc)) {
    data_tbl %>% readr::write_csv(file = save_loc)
  }

  if (format == "wide") {
    data_out <- data_tbl
  }
  if (format == "long") {
    data_out <- data_tbl %>% tsbox::ts_long()
  }
  if (format == "xts") {
    data_out <- data_tbl %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }
  return(data_out)
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
#' @param ser_in series names (character "mnemonic_loc", "mnemonic_AT_loc_freq", "mnemonic__loc_freq", mnemonic@loc.freq")
#' @param freq frequency of the series, required if not contained in the series
#'   name (character "D", "W", "M", "Q", "S", "A")
#'
#' @return series names following udaman convention "mnemonic@loc.freq"
#' @export
#'
#' @examples
#' rename_udaman(c("E_NF_HI", "ECT_HI", "E_TU_HAW"), freq = "M")
#' rename_udaman(c("E_NF__HI_M", "ECT__HI_M", "VAP__HAW_W"))
#' rename_udaman(c("E_NF_AT_HI_M", "ECT_AT_HI_M", "VAP_AT_HAW_W"))
#' rename_udaman(c("E_NF@HI.M", "ECT@HI.M", "VAP@HAW.W"))
#' rename_udaman(c("SH_US@HI.M", "SH_JP__HON_M"))
#' rename_udaman(c("E_NF_HI5", "ECT__HIALL", "E_TU@CNTY"), freq = "M")
#' rename_udaman(c("E_NF_HI5 , ECT__HIALL  E_TU@CNTY", "VAP_HAW ; ECT_HON"), freq = "M")
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
  if (stringr::str_detect(ser_in, "[,;\\s]") %>% any()) {
    ser_in <- stringr::str_split(ser_in, "[,;\\s]+") %>%
      purrr::list_c()
  }
  ser_in <- stringr::str_replace(ser_in, "_AT_", "__")
  mnemonic <- ser_in %>%
    stringr::str_extract(
      "[[:alnum:]_]+(?=_+HI5|_+HI3|_+HIALL|_+HI|_+CNTY|_+HON|_+HAW|_+MAUI|_+MOL|_+LAN|_+MAU|_+KAU|_+NBI|_+US|_+JP|@)"
    ) %>%
    stringr::str_replace("_$", "")
  loc <- ser_in %>%
    {
      ifelse(
        stringr::str_detect(., "@"),
        stringr::str_extract(
          .,
          "@HI5|@HI3|@HIALL|@HI|@CNTY|@HON|@HAW|@MAUI|@MOL|@LAN|@MAU|@KAU|@NBI|@US|@JP"
        ),
        stringr::str_extract(
          .,
          "_+HI5|_+HI3|_+HIALL|_+HI|_+CNTY|_+HON|_+HAW|_+MAUI|_+MOL|_+LAN|_+MAU|_+KAU|_+NBI|_+US|_+JP"
        )
      )
    } %>%
    stringr::str_replace("_+", "@")
  freq <- if (!is.null(freq)) {
    stringr::str_replace(freq, "^", ".")
  } else {
    ser_in %>%
      stringr::str_extract(
        "_D$|_W$|_M$|_Q$|_S$|_A$|.D$|.W$|.M$|.Q$|.S$|.A$"
      ) %>%
      stringr::str_replace("_", ".")
  }

  # function to expand location abbreviations
  expand_loc <- function(ser_id) {
    mnemonic <- stringr::str_extract(ser_id, "[[:alnum:]_]+(?=@)")
    loc <- stringr::str_extract(ser_id, "(?<=@)[[:alnum:]]+(?=\\.)")
    freq <- stringr::str_extract(ser_id, "(?<=\\.)[[:alpha:]]")
    if (loc == "HI5") {
      loc <- c("HI", "HON", "HAW", "MAU", "KAU")
    } else if (loc == "HI3") {
      loc <- c("HI", "HON", "NBI")
    } else if (loc == "HIALL") {
      loc <- c("HI", "HON", "HAW", "MAU", "KAU", "MAUI", "MOL", "LAN")
    } else if (loc == "CNTY") {
      loc <- c("HON", "HAW", "MAU", "KAU")
    }
    return(stringr::str_c(mnemonic, "@", loc, ".", freq))
  }

  ser_ids <- stringr::str_c(mnemonic, loc, freq) %>%
    purrr::map(expand_loc) %>%
    purrr::list_c()

  return(ser_ids)
}


#' Set udaman token for API access
#'
#' @param key a string containing 44 characters
#'
#' @return true if setting the token in .Renviron succeeded
#' @export
#'
#' @details Save the token in .Renviron as udaman_token = key.
#'
#' @examplesIf interactive()
#' set_udaman_token("-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890=")
set_udaman_token <- function(key) {
  if (!rlang::is_string(key)) {
    rlang::abort("`key` must be a string.")
  }
  if (nchar(key) != 44) {
    rlang::warn("`key` does not have a length of 44 characters.")
  }
  set_api_key("udaman", key)
}
