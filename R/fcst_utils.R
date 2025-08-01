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
  json <- httr::content(req, as = "text")
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
  json <- httr::content(req, as = "text")
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


#' Create xts and fill with values
#'
#' @param start date of series start (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param end date of series end (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param per periodicity of series (character: "year" - default)
#'   if date format of start is quarterly, automatically set to "quarter"
#' @param val values to fill in (numeric scalar, vector, or tibble)
#'
#' @return an xts series
#' @export
#'
#' @details when end is missing, but val is a vector of more than one element,
#' the end date is automatically determined by the length of the val vector.
#' if end is missing and val is a scalar, the end date is set to bnk_end.
#' if end is missing the remaining arguments have to be named. if val is a tibble,
#' the end date is automatically determined by the number of rows in the tibble.
#'
#' @examples
#' make_xts()
#' make_xts(val = 0, per = "m")
#' make_xts(start = 20100101, per = "quarter", val = 0)
#' make_xts(start = 2010.1, per = "q", val = 1:10)
#' make_xts(2010.1, val = 1:10) # automatically set per = "quarter"
#' make_xts(start = "2010-01-01", per = "m", val = 0)
#' make_xts(start = 201001, per = "q",
#'          val = tibble::tibble(E_NF_HON = c(1:10), ECT_HI = c(11:20)))
make_xts <- function(
  start = bnk_start,
  end = NULL,
  per = "year",
  val = NA_real_
) {
  if (nchar(as.character(start)) > 4 & nchar(as.character(start)) < 8) {
    per <- "quarter"
  }
  start <- to_ymd(start)
  end <- if (!is.null(end)) to_ymd(end) else NULL
  if ("tbl_df" %in% class(val)) {
    dplyr::bind_cols(
      tibble::tibble(
        time = seq.Date(from = start, by = per, length.out = nrow(val))
      ),
      val
    ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  } else if (is.null(end) & length(val) == 1) {
    tibble::tibble(
      time = seq.Date(from = start, to = bnk_end, by = per),
      value = val
    ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  } else if (is.null(end) & length(val) > 1) {
    tibble::tibble(
      time = seq.Date(from = start, by = per, along.with = val),
      value = val
    ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  } else {
    tibble::tibble(
      time = seq.Date(from = start, to = end, by = per),
      value = val
    ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }
}


#' Create xts addfactor
#'
#' @param start start date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param end end date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param from first value for linear interpolation (numeric)
#' @param to last value for linear interpolation (numeric)
#' @param ser_name name of the xts series (string)
#' @param per periodicity of series (character: "year" - default)
#'   if date format of start is quarterly, automatically set to "quarter"
#'
#' @return a single xts series spanning bnk_start-bnk_end
#' @export
#'
#' @details this is a wrapper around make_xts with some additional functionality.
#' the start and end dates specify the span of the non-zero add-factor value. the
#' remaining period between start and end is filled with zeros.
#'
#' @examples
#' addf()
#' addf(201002, 201504, 1, 2)
#' addf(20100101, 20601201, 1, 2, per = "month")
#' addf(20100101, from = 1, to = 2, per = "quarter")
#' addf(2010.2, 2015.4, 1, 2, "ECT_HI")
addf <- function(
  start = bnk_start,
  end = bnk_end,
  from = 0,
  to = 0,
  ser_name = "value",
  per = "year"
) {
  if (
    nchar(as.character(start)) > 4 &
      nchar(as.character(start)) < 8 |
      per == "quarter"
  ) {
    per <- "quarter"
    start <- to_ymd(start) %>% lubridate::quarter(type = "year.quarter")
    end <- to_ymd(end) %>% lubridate::quarter(type = "year.quarter")
    make_xts(
      start,
      end,
      per = per,
      val = seq.int(from, to, length.out = nqtrs(start, end))
    ) %>%
      tsbox::ts_bind(make_xts(val = 0, per = per)) %>%
      magrittr::set_names(ser_name)
  } else {
    start <- to_ymd(start)
    end <- to_ymd(end) %>% lubridate::floor_date(unit = per)
    make_xts(
      start,
      end,
      per = per,
      val = seq.int(
        from,
        to,
        length.out = lubridate::time_length(end - start, unit = per) %>%
          round() +
          1
      )
    ) %>%
      tsbox::ts_bind(make_xts(val = 0, per = per)) %>%
      magrittr::set_names(ser_name)
  }
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
  in_list <- x_mod %>%
    tidyr::drop_na() %>%
    tsbox::ts_tslist()

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
      purrr::pluck(ser_i) %>%
      tsbox::ts_tbl() %>%
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


#' Set class attribute to tslist
#'
#' @param x list, typically a result of purrr::map() applied to a tslist
#'
#' @return list with class attributes set to list and tslist
#' @export
#'
#' @details A purrr::map() function applied to a tslist (obtained by tsbox::ts_tslist())
#' drops the tslist class attribute. This function resets that attribute.
#'
#' @examples
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_tslist() |>
#'   purrr::map(~ .x / 1000) |>
#'   set_attr_tslist() |>
#'   tsbox::ts_tbl() |>
#'   tsbox::ts_wide()
set_attr_tslist <- function(x) {
  magrittr::set_attr(x, "class", c("list", "tslist"))
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
#     rlang::abort(
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
#     rlang::abort(
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
    rlang::abort(
      "No [time] column detected. ",
      "To be explicit, name time column 'time'."
    )
  }

  time.pos <- which(all.names == time.name)
  id.names <- setdiff(all.names[1:time.pos], time.name)
  value.names <- setdiff(all.names[time.pos:length(all.names)], time.name)

  # character cols or factors should be considered ids, with message
  value.classes <- vapply(x[, value.names], class, "")
  value.names.that.are.ids <- names(value.classes)[
    value.classes %in% c("character", "factor")
  ]

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
    rlang::abort(
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


#' Convert "ts-boxable" objects to long format (extension of `tsbox::ts_long()`)
#'
#' @param x a "ts-boxable" object to be converted
#' @param ser_info should additional details be returned (TRUE) or
#' only the long format of x (default: FALSE)
#'
#' @return returns a ts-boxable object in long format with `id`, `time` and
#' `value` columns. if `ser_info = TRUE`, also returns the following attributes:
#' `was_wide` is `TRUE` if x is a wide data frame, `FALSE` otherwise, and
#' `ser_names` are the names of the series in x.
#' @export
#'
#' @details This function performs a similar operation to `tsbox::ts_long()`. It
#' converts wide data frames and other ts-boxable objects to the long format
#' (wide data frames are not ts-boxable). An important difference compared with
#' `tsbox::ts_long()` is that `conv_long()` ensures that objects containing a single
#' time series get an id column.
#'
#' @examples
#' quarterly_data_example |>
#'   conv_long()
#' quarterly_data_example |>
#'   conv_long() |>
#'   tsbox::ts_tslist() |>
#'   conv_long()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   conv_long(ser_info = TRUE)
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
conv_long <- function(x, ser_info = FALSE) {
  # need xts series names for differential treatment of univariate data
  ser_names_1 <- names(x)
  # check if wide table
  was_wide <- is_wide(x)
  # convert to long table (all formats incl. xts)
  x_mod <-
    {
      if (was_wide) tsbox::ts_long(x) else tsbox::ts_tbl(x)
    }
  # need long tbl series names for differential treatment of univariate data
  ser_names_2 <- tsbox::ts_summary(x_mod) %>%
    dplyr::pull(.data$id)
  ser_names <- if (length(ser_names_1) == 1) {
    ser_names_1
  } else if (length(ser_names_2) == 1) {
    ser_names_2
  } else {
    ser_names_2
  }
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

  if (ser_info) {
    attr(x_mod, "was_wide") <- was_wide
    attr(x_mod, "ser_names") <- ser_names
  }

  return(x_mod)
}


#' Convert "ts-boxable" objects to wide format (extension of `tsbox::ts_wide()`)
#'
#' @param x a "ts-boxable" object to be converted
#'
#' @return returns an object in wide format with a `time` column and series
#' values in subsequent columns with `id` in column heading.
#' @export
#'
#' @details This function performs a similar operation to `tsbox::ts_wide()`. It
#' converts ts-boxable objects to the wide format. An important difference
#' compared with `tsbox::ts_wide()` is that `conv_wide()` does not require x
#' to be a long tbl.
#'
#' @examples
#' quarterly_data_example |>
#'   conv_tslist() |>
#'   conv_wide()
#' quarterly_data_example |>
#'   conv_xts() |>
#'   conv_wide()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   tsbox::ts_xts() |>
#'   conv_wide()
conv_wide <- function(x) {
  # first convert to long and then to wide
  x_mod <- x %>%
    conv_long() %>%
    tsbox::ts_wide()

  return(x_mod)
}


#' Convert "ts-boxable" objects to xts format (extension of `tsbox::ts_xts()`)
#'
#' @param x a "ts-boxable" object to be converted
#'
#' @return returns an object in xts format.
#' @export
#'
#' @details This function performs a similar operation to `tsbox::ts_xts()`. It
#' converts ts-boxable objects to the xts format. An important difference
#' compared with `tsbox::ts_xts()` is that the x argument of `conv_xts()`
#' can be a wide tbl.
#'
#' @examples
#' quarterly_data_example |>
#'   conv_tslist() |>
#'   conv_xts()
#' quarterly_data_example |>
#'   conv_wide() |>
#'   conv_xts()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   conv_wide() |>
#'   conv_xts()
conv_xts <- function(x) {
  # first convert to long and then to xts
  x_mod <- x %>%
    conv_long() %>%
    tsbox::ts_xts()

  return(x_mod)
}


#' Convert "ts-boxable" objects to tslist (extension of `tsbox::ts_tslist()`)
#'
#' @param x a "ts-boxable" object to be converted
#'
#' @return returns an object as a tslist.
#' @export
#'
#' @details This function performs a similar operation to `tsbox::ts_tslist()`.
#' It converts ts-boxable objects to the tslist format. An important difference
#' compared with `tsbox::ts_tslist()` is that the x argument of `conv_tslist()`
#' can be a wide tbl.
#'
#' @examples
#' quarterly_data_example |>
#'   conv_xts() |>
#'   conv_tslist()
#' quarterly_data_example |>
#'   conv_wide() |>
#'   conv_tslist()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   conv_wide() |>
#'   conv_tslist()
conv_tslist <- function(x) {
  # first convert to long and then to tslist
  x_mod <- x %>%
    conv_long() %>%
    tsbox::ts_tslist()

  return(x_mod)
}


# **************************
# time series utility functions ----
# **************************

#' Interpolate a single time series from low to high frequency
#'
#' @param x a single time series (e.g. xts) at low freq (e.g. annual or quarterly)
#' @param x_name the name of the time series x
#' @param conv_type match the interpolated value via "first", "last", "sum",
#' "mean". If conv_type == "uhero" then the name of the time series x is
#' compared to the internal variable `sum_pattern`. For matching series names
#' the interpolation is based on "sum"; for all others it is based on "mean."
#' @param target_freq target frequency "quarter" (default) or "month"
#' @param pattern a single pattern series that the interpolation should follow
#'
#' @return time series at the target frequency
#'
#' @noRd
#' @examplesIf interactive()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_ts() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   disagg_1(conv_type = "mean", target_freq = "month", pattern = NULL) |>
#'   tsbox::ts_plot()
disagg_1 <- function(x, x_name, conv_type, target_freq, pattern) {
  if (is.null(pattern)) {
    formula <- stats::as.formula("x ~ 1")
  } else {
    start <- find_start(x)
    end <- find_end(x, last_day = TRUE)
    pattern <- pattern %>% tsbox::ts_span(start, end)
    formula <- stats::as.formula("x ~ pattern")
  }
  if (stringr::str_to_lower(conv_type) %>% stringr::str_detect("^u")) {
    if (stringr::str_to_upper(x_name) %>% stringr::str_detect(sum_pattern)) {
      conv_type <- "sum"
    } else {
      conv_type <- "mean"
    }
  }
  tempdisagg::td(
    formula = formula,
    conversion = conv_type,
    to = target_freq,
    method = "fast"
  ) %>%
    stats::predict()
}


#' Interpolate univariate or multivariate time series from low to high frequency
#'
#' @param x a tx-boxable object at a low frequency (e.g. annual or quarterly)
#' @param conv_type match the interpolated value via "first", "last", "sum",
#' "mean". If conv_type == "uhero" then the name of the time series x is
#' compared to the internal variable `sum_pattern`. For matching series names
#' the interpolation is based on "sum"; for all others it is based on "mean."
#' @param target_freq target frequency "quarter" or "month"
#' @param pattern a single high-frequency pattern that the interpolation should follow
#'
#' @return interpolated object of the same type as the input
#' @export
#'
#' @details the time-span of the high-frequency pattern has to match or be larger
#' than the time-span of the low frequency series. NA values are not allowed.
#'
#' @examples
#' quarterly_data_example |>
#'   disagg(conv_type = "mean", target_freq = "month")
#' quarterly_data_example |>
#'   disagg(conv_type = "mean", target_freq = "month") |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_frequency(to = "quarter", aggregate = "mean") |>
#'   tsbox::ts_wide() # this matches original data
#' # works with a single series too
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   disagg(conv_type = "mean", target_freq = "month") |>
#'   tsbox::ts_plot()
#' # using a high-frequency pattern
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_span("2005-01-01", "2020-01-01") |>
#'   disagg(
#'     conv_type = "mean", target_freq = "month", pattern = monthly_data_example |>
#'       tsbox::ts_long() |>
#'       tsbox::ts_pick("VISNS_HI")
#'   )
#' # multiple low-frequency series, same number of high-frequency patterns
#' purrr::map2(
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pick("E_NF_HI", "ECT_HI") |>
#'     tsbox::ts_span("2005-01-01", "2020-01-01") |>
#'     tsbox::ts_tslist(),
#'   monthly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pick("VISNS_HI", "VAPNS_HI") |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_tslist(),
#'   ~ disagg(.x, conv_type = "mean", target_freq = "month", pattern = .y)
#' )
disagg <- function(
  x,
  conv_type = "mean",
  target_freq = "quarter",
  pattern = NULL
) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)
  pattern_mod <- if (is.null(pattern)) {
    pattern
  } else {
    conv_long(pattern) %>% tsbox::ts_ts()
  }

  # convert to tslist and interpolate
  x_mod_int <- x_mod %>%
    # tidyr::drop_na() %>%
    tsbox::ts_tslist() %>%
    purrr::map2(
      names(.),
      .f = ~ disagg_1(
        .x,
        .y,
        conv_type = conv_type,
        target_freq = target_freq,
        pattern = pattern_mod
      )
    ) %>%
    set_attr_tslist() %>%
    # univariate data requires special treatment
    {
      if (length(attr(x_mod, "ser_names")) == 1) {
        tsbox::ts_tbl(.) %>%
          tsbox::ts_long() %>%
          dplyr::mutate(id = attr(x_mod, "ser_names"))
      } else {
        tsbox::ts_tbl(.)
      }
    }

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_int)
  } else {
    tsbox::copy_class(x_mod_int, x)
  }

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
  ser_out_dates <- tibble::tibble(
    time = seq.Date(
      from = tsbox::ts_summary(ser_in)$start,
      to = tsbox::ts_summary(ser_in)$end %>%
        lubridate::ceiling_date(unit = "year") %>%
        lubridate::rollback(),
      by = "quarter"
    )
  )
  ser_out <- dplyr::left_join(ser_out_dates, ser_in %>% tsbox::ts_tbl()) %>%
    tidyr::fill(.data$value) %>%
    tsbox::ts_xts() %>%
    magrittr::set_names(ser_out_name)
  dat_start <- tsbox::ts_summary(ser_out)$start
  dat_end <- tsbox::ts_summary(ser_out)$end
  increment <- (tsbox::ts_lag(ser_out, -4) - ser_out) / 4
  increment <- increment %>%
    tsbox::ts_bind(tsbox::ts_lag(increment, 4)[stringr::str_c(
      dat_end - months(9),
      "/",
      dat_end
    )])
  ser_out[stringr::str_c(
    dat_start + months(3),
    "/",
    dat_end
  )] <- (as.numeric(ser_out[dat_start]) +
    tsbox::ts_lag(increment, 1)[stringr::str_c(
      dat_start + months(3),
      "/",
      dat_end
    )] %>%
      cumsum()) %>%
    as.numeric()
  ser_out <- ser_out - 1.5 * increment
  if (aggr != "mean") {
    ser_out <- ser_out / 4
  }
  colnames(ser_out) <- colnames(ser_in) %>%
    stringr::str_replace_all(".SOLA", ".SOLQ") %>%
    stringr::str_replace_all(".A", ".Q")
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
    stringr::str_replace_all(".SOLQ", ".SOLA") %>%
    stringr::str_replace_all(".Q", ".A")
  return(ser_out)
}


# #' Aggregate univariate or multivariate time series from low to high frequency
# #'
# #' @param x a tx-boxable object at a high frequency (e.g. monthly or quarterly)
# #' @param conv_type match the aggregated value via "first", "last", "sum",
# #' "mean". If conv_type == "uhero" then the name of the time series x is
# #' compared to the internal variable `sum_pattern`. For matching series names
# #' the aggregation is based on "sum"; for all others it is based on "mean."
# #' @param target_freq target frequency "year", "quarter", "month", "week"
# #' @param na_rm	logical, if TRUE, incomplete periods are aggregated as
# #' well. For irregular series, incomplete periods are always aggregated.
# #'
# #' @return aggregated object of the same type as the input
# #' @export
# #'
# #' @examples
# #' monthly_data_example |>
# #'   aggr(conv_type = "uhero", target_freq = "quarter")
# #' monthly_data_example |>
# #'   aggr(conv_type = "uhero", target_freq = "quarter") |>
# #'   tsbox::ts_long() |>
# #'   disagg(conv_type = "uhero", target_freq = "month") |>
# #'   tsbox::ts_wide() # this is close to original data
# #' # works with a single series too
# #' monthly_data_example |>
# #'   tsbox::ts_long() |>
# #'   tsbox::ts_pick("VISNS_HI") |>
# #'   aggr(conv_type = "uhero", target_freq = "year") |>
# #'   tsbox::ts_plot()
# aggr <- function(
#   x,
#   conv_type = "mean",
#   target_freq = "year",
#   na_rm = FALSE
# ) {
#   # convert to long format and return additional details
#   x_mod <- conv_long(x, ser_info = TRUE)

#   # convert to tslist and interpolate
#   x_mod_tslist <- x_mod %>%
#     tsbox::ts_tslist()

#   x_mod_names <- x_mod_tslist %>%
#     names() %>%
#     stringr::str_to_upper()

#   if (stringr::str_to_lower(conv_type) %>% stringr::str_detect("^u")) {
#     agg_type <- dplyr::if_else(
#       x_mod_names %>% stringr::str_detect(sum_pattern),
#       "sum",
#       "mean"
#     )
#   } else {
#     agg_type <- conv_type
#   }

#   x_mod_agg <- x_mod_tslist %>%
#     purrr::map2(
#       agg_type,
#       .f = ~ tsbox::ts_frequency(
#         .x,
#         to = target_freq,
#         aggregate = .y,
#         na.rm = na_rm
#       )
#     ) %>%
#     set_attr_tslist() %>%
#     # univariate data requires special treatment
#     {
#       if (length(attr(x_mod, "ser_names")) == 1) {
#         tsbox::ts_tbl(.) %>%
#           tsbox::ts_long() %>%
#           dplyr::mutate(id = attr(x_mod, "ser_names"))
#       } else {
#         tsbox::ts_tbl(.)
#       }
#     }

#   # reclass the output to match the input
#   ans <- if (attr(x_mod, "was_wide")) tsbox::ts_wide(x_mod_agg) else
#     tsbox::copy_class(x_mod_agg, x)

#   return(ans)
# }

#' Aggregate univariate or multivariate time series from low to high frequency
#'
#' @param x a tx-boxable object at a high frequency (e.g. monthly or quarterly)
#' @param conv_type match the aggregated value via "first", "last", "sum",
#' "mean". If conv_type == "uhero" then the name of the time series x is
#' compared to the internal variable `sum_pattern`. For matching series names
#' the aggregation is based on "sum"; for all others it is based on "mean."
#' @param target_freq target frequency "year", "quarter", "month", "week"
#' @param na_rm	logical, if TRUE, incomplete periods are aggregated as
#' well. For irregular series, incomplete periods are always aggregated.
#'
#' @return aggregated object of the same type as the input
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   aggr(conv_type = "sum", target_freq = "quarter")
#' monthly_data_example |>
#'   aggr(conv_type = "uhero", target_freq = "quarter")
#' monthly_data_example |>
#'   aggr(conv_type = "uhero", target_freq = "quarter") |>
#'   tsbox::ts_long() |>
#'   disagg(conv_type = "uhero", target_freq = "month") |>
#'   tsbox::ts_wide() # this is close to original data
#' # works with a single series too
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   aggr(conv_type = "uhero", target_freq = "year") |>
#'   tsbox::ts_plot()
aggr <- function(
  x,
  conv_type = "mean",
  target_freq = "year",
  na_rm = FALSE
) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # convert to nested tibble and aggregate
  x_mod_agg <- x_mod %>%
    tidyr::nest(data = c("id":"value"), .by = "id") %>%
    dplyr::mutate(
      # add a column with aggregation type
      agg_type = if (
        stringr::str_to_lower(conv_type) %>% stringr::str_detect("^u")
      ) {
        dplyr::if_else(
          .data$id %>% stringr::str_detect(sum_pattern),
          "sum",
          "mean"
        )
      } else {
        conv_type
      }
    ) %>%
    # operation on the nested column
    dplyr::rowwise() %>%
    dplyr::mutate(
      aggd = list(tsbox::ts_frequency(
        .data$data,
        to = target_freq,
        aggregate = .data$agg_type,
        na.rm = na_rm
      ))
    ) %>%
    # only keep the aggregated data
    dplyr::select("aggd") %>%
    tidyr::unnest("aggd")

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_agg)
  } else {
    tsbox::copy_class(x_mod_agg, x)
  }

  return(ans)
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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ytd <- x_mod %>%
    dplyr::mutate(yr = lubridate::floor_date(.data$time, "year")) %>%
    dplyr::group_by(.data$id, .data$yr) %>%
    dplyr::mutate(
      value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"yr")

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_ytd)
  } else {
    tsbox::copy_class(x_mod_ytd, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ytd_gr <- x_mod %>%
    ytd_cum() %>%
    tsbox::ts_pcy()

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_ytd_gr)
  } else {
    tsbox::copy_class(x_mod_ytd_gr, x)
  }

  return(ans)
}


#' Fiscal year to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return year to date average, if FALSE, return year to date sum
#'
#' @return object of the same type as the input containing year to date sum or average
#' @export
#'
#' @details this function operates similarly to ytd_cum() but assumes that the
#' fiscal year starts in July and accumulates the values from that month onward.
#'
#' @examples
#' monthly_data_example |>
#'   fytd_cum()
#' quarterly_data_example |>
#'   fytd_cum()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   fytd_cum(avg = FALSE) |>
#'   tsbox::ts_plot()
fytd_cum <- function(x, avg = TRUE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_fytd <- x_mod %>%
    tsbox::ts_lag("6 months") %>%
    dplyr::mutate(yr = lubridate::floor_date(.data$time, "year")) %>%
    dplyr::group_by(.data$id, .data$yr) %>%
    dplyr::mutate(
      value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"yr") %>%
    tsbox::ts_lag("-6 months")

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_fytd)
  } else {
    tsbox::copy_class(x_mod_fytd, x)
  }

  return(ans)
}


#' Fiscal year to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing year to date growth rate
#' @export
#'
#' @details this function operates similarly to ytd_gr() but assumes that the
#' fiscal year starts in July and accumulates the values from that month onward.
#'
#' @examples
#' quarterly_data_example |>
#'   fytd_gr()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   fytd_gr() |>
#'   tail()
fytd_gr <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_fytd_gr <- x_mod %>%
    fytd_cum() %>%
    tsbox::ts_pcy()

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_fytd_gr)
  } else {
    tsbox::copy_class(x_mod_fytd_gr, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_mtd <- x_mod %>%
    dplyr::mutate(yrmo = lubridate::floor_date(.data$time, "month")) %>%
    dplyr::group_by(.data$id, .data$yrmo) %>%
    dplyr::mutate(
      value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"yrmo")

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_mtd)
  } else {
    tsbox::copy_class(x_mod_mtd, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_mtd_gr <- x_mod %>%
    mtd_cum() %>%
    tsbox::ts_pcy()

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_mtd_gr)
  } else {
    tsbox::copy_class(x_mod_mtd_gr, x)
  }

  return(ans)
}


#' Period to date sum or average
#'
#' @param x a ts-boxable object
#' @param per period over which the sum or average of a higher frequency series in x is calculated
#'   (this is the unit of time at which the aggregation is performed:
#'   for ytd per = "year" (default), for mtd per = "month")
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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ptd <- x_mod %>%
    dplyr::mutate(time_per = lubridate::floor_date(.data$time, per)) %>%
    dplyr::group_by(.data$id, .data$time_per) %>%
    dplyr::mutate(
      value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!"time_per")

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_ptd)
  } else {
    tsbox::copy_class(x_mod_ptd, x)
  }

  return(ans)
}


#' Period to date growth rate
#'
#' @param x a ts-boxable object
#' @param per period over which the sum or average of a higher frequency series in x is calculated
#'   (this is the unit of time at which the aggregation is performed:
#'   for ytd per = "year" (default), for mtd per = "month")
#' @param lag_length lag over which growth is calculated or time difference relative to base period
#'   (e.g. "1 year" (default), "3 years", etc. See ?ts_lag() for options)
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
#' # don't use lag_length = "1 year" with weekly data
#' daily_data_example |>
#'   ptd_gr("week")
#' # lag_length = "52 weeks" instead
#' daily_data_example |>
#'   ptd_gr("week", "52 weeks")
#' # and use lag_length = "364 days" with daily data
#' daily_data_example |>
#'   ptd_gr("day", "364 days")
#' daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   ptd_gr("week", "4 weeks") %>%
#'   tail()
ptd_gr <- function(x, per = "year", lag_length = "1 year") {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ptd_gr <- x_mod %>%
    ptd_cum(per) %>%
    {
      (. %ts/% tsbox::ts_lag(., lag_length) %ts-% 1) %ts*% 100
    }

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_ptd_gr)
  } else {
    tsbox::copy_class(x_mod_ptd_gr, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_pc <- x_mod %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^(1 / freq) - 1) * 100)

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_pc)
  } else {
    tsbox::copy_class(x_mod_pc, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_pc <- x_mod %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^freq - 1) * 100)

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_pc)
  } else {
    tsbox::copy_class(x_mod_pc, x)
  }

  return(ans)
}


#' Calculate multi-period average growth
#'
#' @param x ts-boxable object for which growth is calculated (in levels)
#' @param lag number of periods over which growth is calculated
#' @param comp_freq compounding frequency (1 if period by period, 4 if annualized for quarterly data, etc.)
#'
#' @return object of the same type as the input ontaining the average growth of x (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   pcmp(20) |>
#'   tail()
#' quarterly_data_example |>
#'   pcmp(4, 4) |>
#'   tail()
#' quarterly_data_example |>
#'   pcmp(1, 4) |>
#'   tail()
pcmp <- function(x, lag = 4, comp_freq = 1) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # x_mod_pcmp <- x_mod %>%
  #   tsbox::ts_tslist() %>%
  #   purrr::map(
  #     ~ (((.x / tsbox::ts_lag(.x, lag))^(comp_freq / lag)) - 1) * 100
  #   ) %>%
  #   set_attr_tslist() %>%
  #   # univariate data requires special treatment
  #   {
  #     if (length(attr(x_mod, "ser_names")) == 1) {
  #       tsbox::ts_tbl(.) %>%
  #         tsbox::ts_long() %>%
  #         dplyr::mutate(id = attr(x_mod, "ser_names"))
  #     } else {
  #       tsbox::ts_tbl(.)
  #     }
  #   }

  # convert to nested tibble and calculate
  x_mod_pcmp <- x_mod %>%
    tidyr::nest(data = c("id":"value"), .by = "id") %>%
    # operation on the nested column
    dplyr::rowwise() %>%
    dplyr::mutate(
      calc = list((((.data$data %ts/% tsbox::ts_lag(.data$data, lag)))))
      #^(comp_freq / lag)) - 1) * 100)
    ) %>%
    # only keep the calculated data
    dplyr::select("calc") %>%
    tidyr::unnest("calc") %>%
    dplyr::mutate(value = ((.data$value^(comp_freq / lag)) - 1) * 100)

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_pcmp)
  } else {
    tsbox::copy_class(x_mod_pcmp, x)
  }

  return(ans)
}


#' Calculate compund annual growth
#'
#' @param x ts-boxable object for which growth is calculated between first and last period
#'
#' @return a tibble with a single row containing the compound annual growth between
#'   the first and last period of x (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   cagr()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   cagr()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   tsbox::ts_span("2000-01-01", "2020-01-01") |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   cagr()
cagr <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # convert to wide format, keep first and last row only
  # calculate time difference in years and ratio of values
  # apply the cagr formula to all data columns
  x_mod_cagr <- x_mod %>%
    tsbox::ts_wide() %>%
    dplyr::slice(1, dplyr::n()) %>%
    dplyr::summarize(
      dplyr::across(
        "time",
        ~ difftime(get(dplyr::cur_column())[2], get(dplyr::cur_column())[1]) %>%
          lubridate::time_length(unit = "years")
      ),
      dplyr::across(
        !"time",
        ~ get(dplyr::cur_column())[2] / get(dplyr::cur_column())[1]
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      !"time",
      ~ (get(dplyr::cur_column())^(1 / .data$time) - 1) * 100
    )) %>%
    dplyr::rename("years_elapsed" = "time")

  return(x_mod_cagr)
}


#' Extend a series using year over year growth
#'
#' @param yoy_gr ts-boxable object containing year over year growth rates
#' @param hist_lev ts-boxable object containing the history in levels
#' for forecast and at least one year of history (in percent)
#' @param smooth_span extent of smoothing between 0-1 (default: 0, no smoothing).
#' Effect depends on the length of the time series. Low smooth_span value may
#' trigger warning, if time series too short.
#'
#' @return object of the same type as hist_lev extended with year over year growth
#' @export
#'
#' @details This function only works for univariate time series and requires
#' that the growth rates are available for at least the last year of history.
#' Year-over-year growth rates propagate the fluctuations of the base period
#' into the extension period. This can be mitigated by smoothing the extension.
#' The inputs should not contain missing values.
#'
#' @examples
#' gr <- quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::filter(id == "E_NF_HI") |>
#'   tsbox::ts_pcy()
#' lev <- quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::filter(id == "ECT_HI")
#' res1 <- yoy_to_lev(gr, lev |> dplyr::filter(time <= "2010-01-01"))
#' res2 <- yoy_to_lev(gr, lev |> dplyr::filter(time <= "2010-01-01"), 1/8)
#' tsbox::ts_plot(lev, res1, res2)
yoy_to_lev <- function(yoy_gr, hist_lev, smooth_span = 0) {
  # convert to long format and return additional details
  yoy_gr_mod <- conv_long(yoy_gr, ser_info = TRUE)
  hist_lev_mod <- conv_long(hist_lev, ser_info = TRUE)

  # find the start and end of the base period
  base_per_end <- find_end(hist_lev_mod)
  base_per_start <- base_per_end - lubridate::years(1)

  # make sure the growth series starts at or before the start of the base period
  if (find_start(yoy_gr_mod) > base_per_start) {
    stop(
      "The start of the growth series is after the start of the base period.
         Extend the growth series backward with 0% growth in the base period."
    )
  }

  # extract the history in the base period
  hist_lev_base <- hist_lev_mod %>%
    dplyr::filter(.data$time > base_per_start, .data$time <= base_per_end) %>%
    dplyr::mutate(month_id = lubridate::month(.data$time))

  # calculate the cumulative growth and levels
  calcs <- yoy_gr_mod %>%
    dplyr::filter(.data$time > base_per_start) %>%
    dplyr::mutate(month_id = lubridate::month(.data$time)) %>%
    # cumulative growth for each period (works for monthly and quarterly data)
    dplyr::mutate(
      cum_gr = cumprod(1 + .data$value / 100),
      .by = "month_id"
    ) %>%
    # store the cumulative growth in the base period
    dplyr::left_join(
      dplyr::filter(
        .,
        .data$time > base_per_start,
        .data$time <= base_per_end
      ) %>%
        dplyr::select("month_id", "base_gr" = "cum_gr"),
      by = "month_id"
    ) %>%
    # add the history in the base period
    dplyr::left_join(
      hist_lev_base %>%
        dplyr::select("month_id", "base_val" = "value"),
      by = "month_id"
    ) %>%
    dplyr::mutate(
      # adjust the cumulative growth so it is 1 in the base period
      cum_gr_adj = .data$cum_gr / .data$base_gr,
      # calculate levels using the adjusted cumulative growth applied to the base period
      lev_val = .data$base_val * .data$cum_gr_adj
    ) %>%
    # select the relevant columns
    dplyr::select("id", "time", "value" = "lev_val") %>%
    # name the growth series the same as the history
    dplyr::mutate(id = attr(hist_lev_mod, "ser_names"))

  # extend history with period to period growth of the calculated levels
  # and apply smoothing to extension if requested
  if (smooth_span == 0) {
    ext_lev_mod <- hist_lev_mod %>%
      tsbox::ts_chain(calcs)
  } else {
    ext_lev_mod <- hist_lev_mod %>%
      tsbox::ts_chain(calcs) %>%
      dplyr::mutate(
        value_smooth = stats::loess(
          value ~ time %>% as.numeric(),
          data = .,
          span = smooth_span
        ) %>%
          stats::predict(),
        value = dplyr::if_else(
          .data$time > base_per_end,
          .data$value_smooth,
          .data$value
        )
      ) %>%
      dplyr::select(!"value_smooth")
  }

  # reclass the output to match the input
  ans <- if (attr(hist_lev_mod, "was_wide")) {
    tsbox::ts_wide(ext_lev_mod)
  } else {
    tsbox::copy_class(ext_lev_mod, hist_lev)
  }

  return(ans)
}


#' Get indexed series (wrapper around tsbox::ts_index())
#'
#' @param x ts-boxable object to be indexed
#' @param base_per base date when the index is set to base_value (see examples).
#'   If two dates are provided, the mean in the range is set equal to base_value.
#' @param base_value numeric value of the index at base_per (e.g. 1 or 100)
#'
#' @return indexed object of the same type as the input
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   index(2010.1)
#' quarterly_data_example |>
#'   index(c(2010.1, 2010.4))
#' quarterly_data_example |>
#'   index(c("2010-01-01", "2010-12-31"), 1)
index <- function(x, base_per = as.character(Sys.Date()), base_value = 100) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_index <- x_mod %>%
    tsbox::ts_index(base = to_ymd(base_per)) %>%
    dplyr::mutate(value = .data$value * base_value) %>%

    # univariate data requires special treatment
    {
      if (length(attr(x_mod, "ser_names")) == 1) {
        tsbox::ts_tbl(.) %>%
          tsbox::ts_long() %>%
          dplyr::mutate(id = attr(x_mod, "ser_names"))
      } else {
        tsbox::ts_tbl(.)
      }
    }

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_index)
  } else {
    tsbox::copy_class(x_mod_index, x)
  }

  return(ans)
}


#' Specify span of time series (wrapper around tsbox::ts_span())
#'
#' @param x ts-boxable object to filter by span
#' @param start start date (see examples)
#' @param end end date (see examples)
#' @param template ts-boxable time series (see tsbox::ts_span)
#' @param extend logical. If true, the start and end values are allowed to extend the series (by adding NA values).
#'
#' @return filtered object of the same type as the input
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   span(2010.1)
#' quarterly_data_example |>
#'   span(2010.1, 2010.4)
#' quarterly_data_example |>
#'   span("2010-01-01", "2010-12-31")
span <- function(x, start = NULL, end = NULL, template = NULL, extend = FALSE) {
  start <- if (!is.null(start)) to_ymd(start)
  end <- if (!is.null(end)) to_ymd(end)

  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_span <- x_mod %>%
    tsbox::ts_span(
      start = start,
      end = end,
      template = template,
      extend = extend
    ) %>%

    # univariate data requires special treatment
    {
      if (length(attr(x_mod, "ser_names")) == 1) {
        tsbox::ts_tbl(.) %>%
          tsbox::ts_long() %>%
          dplyr::mutate(id = attr(x_mod, "ser_names"))
      } else {
        tsbox::ts_tbl(.)
      }
    }

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_span)
  } else {
    tsbox::copy_class(x_mod_span, x)
  }

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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod %>%
    tsbox::ts_summary() %>%
    dplyr::pull(.data$start)
}


#' Find the date of the last observation (NAs are dropped)
#'
#' @param x ts-boxable object
#' @param last_day should the last day of period be returned (default: FALSE)
#'
#' @return date associated with last observation
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   dplyr::mutate(E_NF_HI = dplyr::if_else(time > "2022-01-01", NA_real_, E_NF_HI)) |>
#'   find_end()
#' quarterly_data_example |>
#'   dplyr::mutate(E_NF_HI = dplyr::if_else(time > "2022-01-01", NA_real_, E_NF_HI)) |>
#'   find_end(TRUE)
find_end <- function(x, last_day = FALSE) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_summary <- x_mod %>%
    tsbox::ts_summary()

  x_mod_summary %>%
    dplyr::pull(.data$end) %>%
    {
      if (last_day) {
        purrr::map2_vec(
          .,
          x_mod_summary %>% dplyr::pull(., .data$diff),
          ~ lubridate::ceiling_date(.x, unit = .y) %>%
            lubridate::rollback()
        )
      } else {
        .
      }
    }
}


#' Concatenate dates to obtain period
#'
#' @param dat1 date of period start (string: see examples)
#' @param dat2 date of period end (string: see examples)
#'
#' @return string containing date range
#' @export
#'
#' @examples
#' p("2010-01-01", "2020-01-01")
#' p(20100101, 20200101)
#' p(2010.1, 2020.4)
#' p(,2020.4)
#' p("2010Q1", "2020Q4")
#' p(2010, 2020) # for annual period only
p <- function(dat1 = "", dat2 = "") {
  if (nchar(dat1) != 0) {
    dat1 <- to_ymd(dat1)
  }
  if (nchar(dat2) != 0) {
    dat2 <- to_ymd(dat2)
  }
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
  stringr::str_c(
    if (dat1 != "") dat1 %>% lubridate::ym(),
    "/",
    if (dat2 != "") dat2 %>% lubridate::ym()
  )
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
  stringr::str_c(
    if (dat1 != "") dat1 %>% lubridate::yq(),
    "/",
    if (dat2 != "") dat2 %>% lubridate::yq()
  )
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
  stringr::str_c(
    if (dat1 != "") stringr::str_c(dat1, "-01-01"),
    "/",
    if (dat2 != "") stringr::str_c(dat2, "-01-01")
  )
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


#' Parse strings into dates in yyyy-mm-dd format
#'
#' @param x string (string: yyyymmdd, yyyyqq, yyyy.q, yyyy)
#'
#' @return formatted dates (yyyy-mm-dd)
#' @export
#'
#' @examples
#' to_ymd(c("2010.0211", 202002, 2020.2, "2020"))
to_ymd <- function(x) {
  x %>%
    lubridate::parse_date_time(orders = c("ymd", "yq", "y")) %>%
    lubridate::ymd()
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


#' Calculate number of months between two dates yyyyMm, yyyy.m or yyyy-mm-dd
#'
#' @param dat1 date of period start (string: yyyyMm, yyyy.m, or yyyy-mm-dd)
#' @param dat2 date of period end (string: yyyyMm, yyyy.m, or yyyy-mm-dd)
#'
#' @return numeric length of date range in months
#' @export
#'
#' @details The endpoints are included in the result so subtract one for time difference.
#' Also, the result is rounded down so partial months are not counted. See examples.
#'
#' @examples
#' nmons("2010M1", "2010M2")
#' nmons(2010.1, 2010.4)
#' nmons("2010-01-15", "2010-04-15")
#' nmons("2010-01-15", "2010-04-18")
#' nmons("2010-01-15", "2010-04-12")
nmons <- function(dat1 = "", dat2 = "") {
  dat1 <- to_ymd(dat1)
  dat2 <- to_ymd(dat2)
  xts::nmonths(make_xts(start = dat1, end = dat2, per = "months"))
}


#' Calculate number of quarters between two dates yyyyQq, yyyy.q or yyyy-mm-dd
#'
#' @param dat1 date of period start (string: yyyyQq, yyyy.q, or yyyy-mm-dd)
#' @param dat2 date of period end (string: yyyyQq, yyyy.q, or yyyy-mm-dd)
#'
#' @return numeric length of date range in quarters
#' @export
#'
#' @details The endpoints are included in the result so subtract one for time difference.
#' Also, the result is rounded down so partial quarters are not counted. See examples.
#'
#' @examples
#' nqtrs("2010Q1", "2020Q4")
#' nqtrs(2010.1, 2020.4)
#' nqtrs("2010-01-01", "2020-10-01")
#' nqtrs("2010-02-01", "2020-11-01")
#' nqtrs("2010-02-01", "2020-10-01")
#' nqtrs("2010-01-01", "2020-11-01")
nqtrs <- function(dat1 = "", dat2 = "") {
  dat1 <- to_ymd(dat1)
  dat2 <- to_ymd(dat2)
  xts::nquarters(make_xts(start = dat1, end = dat2, per = "quarters"))
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
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ma <- x_mod %>%
    dplyr::mutate(
      value = slider::slide_dbl(
        .data$value,
        mean,
        .before = order - 1,
        .complete = TRUE
      ),
      .by = "id"
    )

  # reclass the output to match the input
  ans <- if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(x_mod_ma)
  } else {
    tsbox::copy_class(x_mod_ma, x)
  }

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
plot_1 <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_1 = TRUE
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  ser_names_pct <- if (gr_1) {
    stringr::str_glue("{ser_names}%")[1]
  } else {
    stringr::str_glue("{ser_names}%")
  }

  ser_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) {
        tsbox::ts_c(., tsbox::ts_pcy(.))
      } else {
        tsbox::ts_c(., tsbox::ts_pc(.))
      }
    } %>%
    # magrittr::extract(, 1:length(c(ser_names, ser_names_pct))) %>%
    tsbox::ts_pick(1:length(c(ser_names, ser_names_pct))) %>%
    magrittr::set_names(c(ser_names, ser_names_pct)) %>%
    tsbox::ts_dygraphs(
      main = ser_names[1],
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "% change") %>%
    dygraphs::dyAxis(
      "y2",
      label = "level",
      drawGrid = FALSE,
      independentTicks = TRUE
    ) %>%
    {
      if (length(ser_names_pct) > 1) {
        dygraphs::dyGroup(., ser_names_pct, axis = "y") %>%
          dygraphs::dyMultiColumnGroup(ser_names_pct)
      } else {
        dygraphs::dyBarSeries(., ser_names_pct, axis = "y")
      }
    } %>%
    {
      if (length(ser_names) > 1) {
        dygraphs::dyGroup(., ser_names, strokeWidth = 2, axis = "y2")
      } else {
        dygraphs::dySeries(., ser_names, strokeWidth = 2, axis = "y2")
      }
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(
      colors = uh_colors_light[1:length(ser_names_pct)] %>%
        as.vector() %>%
        c(uh_colors[1:length(ser_names)] %>% as.vector())
    ) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

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
plot_2ax <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  ser_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(
      main = ser_names[1] %>% stringr::str_replace_all("@.*", ""),
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "series 1") %>%
    dygraphs::dyAxis(
      "y2",
      label = "series 2+",
      drawGrid = FALSE,
      independentTicks = TRUE
    ) %>%
    {
      dygraphs::dySeries(., ser_names[1], strokeWidth = 2, axis = "y")
    } %>%
    {
      if (length(ser_names[-1]) > 1) {
        dygraphs::dyGroup(., ser_names[-1], strokeWidth = 2, axis = "y2")
      } else {
        dygraphs::dySeries(., ser_names[-1], strokeWidth = 2, axis = "y2")
      }
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(
      colors = uh_colors[1:length(ser_names)] %>% as.vector()
    ) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

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
plot_fc <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  add_table = TRUE,
  table_start = rng_start,
  table_end = rng_end,
  height = 300,
  width = 900,
  yoy_gr = TRUE
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  table_start <- to_ymd(table_start)
  table_end <- to_ymd(table_end)

  # formatting of the date axis
  getQuarter <- 'function(d) {
      d = d || new Date();
      var n = [1,2,3,4];
      var qr = "Q" + n[Math.floor(d.getMonth() / 3)];
//      var twoDigitsYear = parseInt(d.getFullYear().toString().substr(2,2), 10);
      var Year = parseInt(d.getFullYear().toString(), 10);
//     return [twoDigits+qr];
      return [Year+qr];
}'

  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  # series to plot
  ser_to_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) {
        tsbox::ts_c(., tsbox::ts_pcy(.[, 1]))
      } else {
        tsbox::ts_c(., tsbox::ts_pc(.[, 1]))
      }
    } %>%
    magrittr::set_names(c(ser_names, stringr::str_glue("{ser_names[1]}%")))

  # generate the plot
  ser_plot <- ser_to_plot %>%
    tsbox::ts_dygraphs(
      main = ser_names[1],
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "level") %>%
    dygraphs::dyAxis(
      "y2",
      label = "% Chg",
      drawGrid = FALSE,
      independentTicks = TRUE
    ) %>%
    dygraphs::dyAxis("x", axisLabelFormatter = htmlwidgets::JS(getQuarter)) %>%
    dygraphs::dySeries(
      stringr::str_glue("{ser_names[1]}"),
      axis = "y",
      strokeWidth = 2,
      color = uh_colors[1]
    ) %>%
    {
      if (length(ser_names) > 1) {
        dygraphs::dySeries(
          .,
          stringr::str_glue("{ser_names[2]}"),
          axis = "y",
          strokePattern = "dashed",
          strokeWidth = 2,
          color = uh_colors[2]
        )
      } else {
        .
      }
    } %>%
    {
      if (length(ser_names) > 2) {
        dygraphs::dySeries(
          .,
          stringr::str_glue("{ser_names[3]}"),
          axis = "y",
          strokePattern = "dashed",
          strokeWidth = 2,
          color = uh_colors[3]
        )
      } else {
        .
      }
    } %>%
    dygraphs::dySeries(
      stringr::str_glue("{ser_names[1]}%"),
      axis = "y2",
      stepPlot = TRUE,
      fillGraph = TRUE,
      color = uh_colors[1]
    ) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    ) %>%
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
plot_comp_2 <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_bar = FALSE
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  plot_level <-
    x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(
      main = "Level",
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uh_colors[1:length(ser_names)] %>% as.vector()) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_pcy(.) else tsbox::ts_pc(.)
    } %>%
    tsbox::ts_dygraphs(
      main = "Growth",
      group = "comp",
      height = height,
      width = width
    ) %>%
    {
      if (gr_bar) dygraphs::dyBarChart(.) else .
    } %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(
      colors = uh_colors[1:length(ser_names)] %>% as.vector()
    ) %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2")) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

  # render the dygraphs objects using htmltools
  list(plot_level, plot_growth) %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Three-panel plot of levels, index, and growth rates
#'
#' @param x ts-boxable object to plot
#' @param base_date base period for the indexed series ("YYYY-MM-DD")
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
plot_comp_3 <- function(
  x,
  base_date = as.character(Sys.Date() - lubridate::years(10)),
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_bar = FALSE
) {
  base_date <- to_ymd(base_date)
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  plot_level <-
    x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(
      main = "Level",
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uh_colors[1:length(ser_names)] %>% as.vector()) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  # plot_level[["elementId"]] <- ser_names %>% extract(1) %>% str_extract("^.*@")
  plot_index <-
    x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_index(base = base_date) %>%
    tsbox::ts_dygraphs(
      main = "Index",
      group = "comp",
      height = height,
      width = width
    ) %>%
    # dygraphs::dyRebase(value = 100) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uh_colors[1:length(ser_names)] %>% as.vector()) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  plot_growth <-
    x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_pcy(.) else tsbox::ts_pc(.)
    } %>%
    tsbox::ts_dygraphs(
      main = "Growth",
      group = "comp",
      height = height,
      width = width
    ) %>%
    {
      if (gr_bar) dygraphs::dyBarChart(.) else .
    } %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(
      colors = uh_colors[1:length(ser_names)] %>% as.vector()
    ) %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2")) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

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
    stringr::str_replace_all(c(
      "<style>" = "\\\n<title>Plots</title>\\\n<style>",
      "\\}</style>" = "font-family:Arial,Helvetica,sans-serif;font-size:medium;}</style>"
    )) %>%
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


#' Parse lm() output and convert into bimets equation (GETS model development)
#'
#' @param model a model estimated by lm() (lm object)
#' @param ... arguments to format the coefficients e.g. digits = 3
#'
#' @return a character vector containing the estimated equation (1) and bimets components (2:5)
#' @export
#'
#' @examplesIf interactive()
#' # this function combines coefficient estimates and variable names into an equation
#' # in vector element 1 and into bimets components in vector elements 2-5.
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
#' # (3) "TSRANGE 1990 1 2010 4"
#' # (4) "EQ> uk = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 2) + b3 * TSLAG(uk, 3) + b4 *
#' # TSLAG(LOG(uk), 5) + b5 * TSLAG(LOG(uk), 6) + b6 * TSLAG(LOG(uk), 12)"
#' # (5) "COEFF> b0 b1 b2 b3 b4 b5 b6"
#' ## regression on multiple lags in a single L() call
#' dfm <- dynlm::dynlm(d(log(uk)) ~ L(uk, c(1, 11, 12)), start = c(1975, 1), end = c(1982, 12))
#' model_equation(dfm)
#' # (1) "d(log(uk)) = 0.1018542 - 0.2379287 * L(uk, c(1, 11, 12))1 + 0.0368355 *
#' # L(uk, c(1, 11, 12))11 + 0.1689896 * L(uk, c(1, 11, 12))12"
#' # (2) "BEHAVIORAL> uk"
#' # (3) "TSRANGE 1990 1 2010 4"
#' # (4) "EQ> TSDELTA(LOG(uk)) = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 11) + b3 * TSLAG(uk, 12)"
#' # (5) "COEFF> b0 b1 b2 b3"
model_equation <- function(model, ...) {
  format_args <- list(...)

  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- dplyr::case_when(
    model_coeff_sign == -1 ~ " - ",
    model_coeff_sign == 1 ~ " + ",
    model_coeff_sign == 0 ~ " + "
  )

  # components of TSRANGE from date stamps of residuals
  model_tsr <- model$residuals %>%
    names() %>%
    lubridate::parse_date_time(c("ymd", "my", "yq", "y")) %>%
    lubridate::ymd()
  model_eqn_tsr <- stringr::str_flatten(
    c(
      "TSRANGE",
      lubridate::year(model_tsr %>% utils::head(1)),
      lubridate::quarter(model_tsr %>% utils::head(1)),
      lubridate::year(model_tsr %>% utils::tail(1)),
      lubridate::quarter(model_tsr %>% utils::tail(1))
    ),
    collapse = " "
  )

  # model equation with estimated coefficients
  model_eqn <- stringr::str_c(
    stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
    " = ",
    stringr::str_c(
      model_coeff_prefix,
      do.call(format, format_args),
      " * ",
      names(model_coeff),
      collapse = ""
    )
  ) %>%
    stringr::str_squish() %>%
    stringr::str_replace("= \\+ ", "= ") %>%
    stringr::str_replace("\\* c |\\* CONST |\\* \\(Intercept\\) ", "* CONST ")

  # formal model equation with b coefficients
  # if there is a constant term in the model (Intercept, CONST, c)
  # move it to the first position with b0 coefficient
  if (
    names(model_coeff) %>% stringr::str_detect("CONST|Intercept|^c$") %>% any()
  ) {
    const_index <- which(
      names(model_coeff) %>% stringr::str_detect("CONST|Intercept|^c$")
    )
    model_eqn_bim <- stringr::str_c(
      stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
      " = ",
      stringr::str_c(
        "b0",
        stringr::str_c(
          stringr::str_c(" + b", 1:length(model_coeff[-1]), sep = ""),
          " * ",
          names(model_coeff[-const_index]),
          collapse = ""
        )
      )
    ) %>%
      stringr::str_squish() %>%
      stringr::str_replace("= \\+ ", "= ")
  } else {
    model_eqn_bim <- stringr::str_c(
      stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
      " = ",
      stringr::str_c(
        stringr::str_c(" + b", 1:length(model_coeff), sep = ""),
        " * ",
        names(model_coeff),
        collapse = ""
      )
    ) %>%
      stringr::str_squish() %>%
      stringr::str_replace("= \\+ ", "= ")
  }

  # format model equation in bimets format
  model_eqn_beh <- stringr::str_extract(
    model_eqn_bim,
    "[_.\\(\\)[:alnum:]]+"
  ) %>% # extract the target variable name
    stringr::str_replace_all("DL_([_.[:alnum:]]+)", "TSDELTALOG(\\1)") %>% # replace DL_ with TSDELTALOG()
    stringr::str_replace_all("L_([_.[:alnum:]]+)", "LOG(\\1)") %>% # replace L_ with LOG()
    stringr::str_replace_all("D_([_.[:alnum:]]+)", "TSDELTA(\\1)") %>% # replace D_ with TSDELTA()
    stringr::str_replace_all("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)") %>% # replace log() with LOG()
    stringr::str_replace_all(
      "d\\(([_., \\(\\)[:alnum:]]+)\\)",
      "TSDELTA(\\1)"
    ) %>% # dynlm::d() = diff()
    stringr::str_replace_all("[\\(]+", "_") %>% # drop parentheses from equation name
    stringr::str_replace_all("[., \\)[:digit:]]+", "") %>% # drop parentheses and extra items from equation name
    stringr::str_replace_all(c(
      "^TSDELTALOG_" = "",
      "^TSDELTA_" = "",
      "^LOG_" = ""
    )) # clean equation name
  # first deal with lags then with other transformations
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "([_[:alpha:]]+)(\\.)([[:digit:]]{1,2})",
    "TSLAG(\\1, \\3)"
  ) # replace dot notation for lags with TSLAG()
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "L\\(([_\\(\\) [:alpha:]]+)([, ]+)([:c\\(, [:digit:]\\)]+)\\)([[:digit:]]{1,2})",
    "TSLAG(\\1, \\4)"
  ) # dealing with dynlm::L(x, 1:4) or dynlm::L(x, c(2,4))
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "L\\(([_., \\(\\)[:alnum:]]+)\\)",
    "TSLAG(\\1)"
  ) # dynlm::L(x, k) = lag(x, -k)
  model_eqn_bim <- model_eqn_bim %>%
    stringr::str_replace_all("DL_([_.[:alnum:]]+)", "TSDELTALOG(\\1)") %>% # replace DL_ with TSDELTALOG()
    stringr::str_replace_all("L_([_.[:alnum:]]+)", "LOG(\\1)") %>% # replace L_ with LOG()
    stringr::str_replace_all("D_([_.[:alnum:]]+)", "TSDELTA(\\1)") %>% # replace D_ with TSDELTA()
    stringr::str_replace_all("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)") %>% # replace log() with LOG()
    stringr::str_replace_all(
      "d\\(([_., \\(\\)[:alnum:]]+)\\)",
      "TSDELTA(\\1)"
    ) %>% # dynlm::d() = diff()
    stringr::str_replace_all("lag\\(([_., \\(\\)[:alnum:]]+)\\)", "TSLAG(\\1)") # lag() with TSLAG()
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "S_(\\d{4})_(\\d{2})_(\\d{2})",
    "S_\\1-\\2-\\3"
  ) %>%
    stringr::str_replace_all(
      "(\\d{4}-\\d{2}-\\d{2})",
      ymd_to_yQq
    ) # convert IIS_1980_10_01 to IIS_1980Q4 and SIS_1980_10_01 to SIS_1980Q4
  model_eqn_coe <- stringr::str_extract_all(
    model_eqn_bim,
    "(b[[:digit:]]+)",
    simplify = TRUE
  ) %>%
    paste(collapse = " ") # # extract coefficients
  model_eqn_beh <- stringr::str_replace_all(model_eqn_beh, "^", "BEHAVIORAL> ") # add a line for BEHAVIORAL>
  model_eqn_bim <- stringr::str_replace_all(model_eqn_bim, "^", "EQ> ") # add EQ> to start of line
  model_eqn_coe <- stringr::str_replace_all(model_eqn_coe, "^", "COEFF> ") # add COEFF> to start of line

  return(c(
    model_eqn,
    model_eqn_beh,
    model_eqn_tsr,
    model_eqn_bim,
    model_eqn_coe
  ))
}


#' Parse gets output and extract underlying data (GETS model development)
#'
#' @param model_in a model estimated by arx, isat, or getsm
#' @param y_name the actual name of the y variable
#'
#' @return an xts containing the model variables
#' @export
#'
#' @examplesIf interactive()
#' # save the data associated with a gets model
extract_data <- function(model_in, y_name) {
  data_out <- gets::eviews(model_in, print = FALSE, return = TRUE)$data %>%
    dplyr::rename_with(~y_name, .cols = 2) %>%
    dplyr::rename_with(
      ~ stringr::str_replace(., "ar", stringr::str_glue("{y_name}."))
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(., c("iis" = "IIS_", "sis" = "SIS_"))
    ) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., "-", "_")) %>%
    dplyr::mutate(
      time = lubridate::ymd(index),
      .before = 1,
      .keep = "unused"
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  return(data_out)
}


#' Update a bimets model with new/modified equations
#'
#' @param model_1 original estimated bimets model
#' @param model_2 estimated bimets model containing updates (only updated
#'   equations need to be estimated)
#' @param eqList names of updated behavioral equations (vector of strings),
#'   others taken from model_1 (equations missing from model_2 are removed)
#'
#' @return estimated bimets model containing updates
#' @export
#'
#' @details Start by making a copy of the original model's equations (txt file).
#' Re-specify some equations, add new equations, and remove not needed equations.
#' Load the new model as model_2 and estimate the modified/new equations (ok to estimate all).
#' Replace the equations in model_2 that should remain the same as in model_1
#' by the estimated equations from model_1. Equations that are to remain unchanged
#' have to be present in both model_1 and model_2, and not present in eqList.
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


#' Set tsrange for behavioral equations to available data range
#'
#' @param model_w_dat bimets model (with data) to be estimated
#' @param max_lag the largest lag (default = 4) in the model (to offset starting point for estimation)
#' @param eqns names of behavioral equations to set tsrange for (default = NULL: all equations)
#'
#' @return bimets model with tsrange set for estimation
#' @export
#'
#' @details Find periods where all variables in the equation are available.
#' Shift beginning of the sample by max_lag periods.
#' Set the tsrange for each equation (used in estimation).
#'
#' @examplesIf interactive()
#' set_tsrange(scen_model_dat, 4)
set_tsrange <- function(model_w_dat, max_lag = 4, eqns = NULL) {
  # if no equations are specified, use all behavioral equations
  if (is.null(eqns)) {
    eqns <- model_w_dat$vendogBehaviorals
  }

  # for each behavioral equation, set the tsrange to the available data
  # bimets::GETRANGE() accomplishes a similar task but does not offset the start by max_lag
  for (eq_i in eqns) {
    model_w_dat$behaviorals[[eq_i]][["tsrange"]] <- model_w_dat$modelData %>%
      magrittr::extract(model_w_dat$behaviorals[[eq_i]][[
        "eqComponentsNames"
      ]]) %>%
      set_attr_tslist() %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      tidyr::drop_na() %>%
      dplyr::slice(1 + max_lag, dplyr::n()) %>%
      dplyr::pull(.data$time) %>%
      purrr::map(~ c(year(.x), quarter(.x))) %>%
      purrr::reduce(c)
  }

  return(model_w_dat)
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
  Sys.setenv(udaman_token = key)
}
