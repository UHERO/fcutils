# **************************
# downloads from outside sources ----
# **************************

# environment variable (and keyring service) holding each source's key
api_key_names <- c(
  fred = "FRED_API_KEY",
  estat = "ESTAT_API_KEY",
  eia = "EIA_API_KEY",
  udaman = "udaman_token"
)


#' Set an API key for a data source
#'
#' @param service "fred", "estat", "eia" or "udaman"
#' @param key the key (string)
#' @param store "session" (default) sets it for this R session only;
#'   "keyring" stores it among the system's credentials (e.g. the macOS
#'   keychain), where later sessions find it
#'
#' @return nothing (the key is stored)
#' @export
#'
#' @details Functions that need a key look first in the environment
#' variable (`FRED_API_KEY`, `ESTAT_API_KEY`, `EIA_API_KEY`, `udaman_token`),
#' which can also be set in `.Renviron`, and then in the keyring under the
#' same name.
#'
#' @examplesIf interactive()
#' set_api_key("fred", "abcdefghijklmnopqrstuvwxyz123456")
#' set_api_key("eia", "abc123", store = "keyring")
set_api_key <- function(service, key, store = c("session", "keyring")) {
  store <- match.arg(store)
  service <- match.arg(service, names(api_key_names))
  if (!rlang::is_string(key) || !nzchar(key)) {
    rlang::abort("`key` must be a non-empty string.")
  }
  var <- api_key_names[[service]]
  if (store == "session") {
    do.call(Sys.setenv, stats::setNames(list(key), var))
  } else {
    keyring::key_set_with_value(service = var, password = key)
  }
  invisible(NULL)
}


#' API key for a data source, from the environment or the keyring
#'
#' @return the key (string); an error saying how to set it if there is none
#' @noRd
api_key <- function(service) {
  var <- api_key_names[[service]]
  key <- Sys.getenv(var)
  if (nzchar(key)) {
    return(key)
  }
  in_keyring <- tryCatch(
    var %in% keyring::key_list(var)$service,
    error = function(e) FALSE
  )
  if (in_keyring) {
    return(keyring::key_get(var))
  }
  rlang::abort(stringr::str_c(
    "No ", service, " key. Add ", var, " to .Renviron, or run ",
    "set_api_key(\"", service, "\", \"<key>\")."
  ))
}


#' Read JSON from a URL, retrying on failure
#'
#' Tries `tries` times, waiting longer each time. Error messages show the URL
#' with any key removed.
#' @noRd
read_json_retry <- function(url, ..., tries = 3, wait = 5) {
  for (i in seq_len(tries)) {
    # the connection's warnings repeat the URL, key included: re-issue them
    # with the key removed
    res <- tryCatch(
      withCallingHandlers(
        jsonlite::fromJSON(url, ...),
        warning = function(w) {
          rlang::warn(mask_keys(conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      return(res)
    }
    if (i < tries) {
      Sys.sleep(wait * i)
    }
  }
  rlang::abort(stringr::str_c(
    "Download failed after ", tries, " tries: ", mask_keys(url), "\n",
    mask_keys(conditionMessage(res))
  ))
}


#' Remove keys from a URL or message
#' @noRd
mask_keys <- function(x) {
  stringr::str_replace_all(x, "(appId|api_key)=[^&\\s]+", "\\1=<key>")
}


#' Download series from outside sources
#'
#' @param id series ids: FRED codes ("UNRATE"), StatCan vectors
#'   ("v2062810"), Bank of Canada Valet series ("FXMUSDCAD"), e-Stat table
#'   ids ("0004052037") or EIA series ids ("WTIPUUS"). Several ids are read
#'   one by one.
#' @param source "fred", "statcan", "boc", "estat" or "eia"
#' @param freq NULL (default) keeps the source's frequency. "month" or
#'   "quarter" averages the data to that frequency, which must be lower than
#'   the source's (daily, weekly or monthly to months or quarters). See
#'   Details for which periods are kept.
#' @param start first date to request (any form `to_ymd()` reads; default
#'   1940). The data run from `start` to the latest date the source has
#'   published; there is no end date.
#' @param filters named list of further query parameters. e-Stat: the table's
#'   category codes, e.g. `list(cdTab = "1", cdCat01 = "0001", cdArea =
#'   "00000")`. EIA: `route` (e.g. "steo" or "aeo/2025"), `frequency`
#'   ("monthly", "annual") and facets such as `scenario = "ref2025"`; EIA
#'   returns at most 5,000 observations per series. FRED: added to the
#'   request as query parameters, e.g. `list(units = "pc1")`. StatCan and
#'   Bank of Canada: ignored.
#' @param format "wide" (default), "long" or "xts", as in `get_series()`
#'
#' @return the series, in the chosen format, named by id
#' @export
#'
#' @details With `freq`, each period is the mean of the observations in it,
#' and these periods are left out:
#' * the current calendar period, which has not ended yet;
#' * a period that `start` falls inside of, e.g. the first quarter when
#'   `start` is February 15;
#' * for monthly (or quarterly) data, the first or last period of a series
#'   when it lacks its first or last month: a quarter with October and
#'   November published but not December. FRED leaves such a quarter out
#'   too.
#'
#' Periods inside a series are kept when an observation is missing; the mean
#' is over the observations present. A quarter without October averages
#' November and December, as FRED does.
#'
#' Keys come from the environment or the keyring (see
#' `set_api_key()`); StatCan and the Bank of Canada need none. Downloads are
#' tried three times. StatCan answers with an empty list during its nightly
#' maintenance; that gives an error saying so. For e-Stat, the filters must
#' leave one value per date; monthly and annual time codes are read.
#'
#' @examplesIf interactive()
#' get_series_src(c("UNRATE", "CIVPART"), "fred", start = 2020)
#' get_series_src("DCOILWTICO", "fred", freq = "month", start = 2024)
#' get_series_src("UNRATE", "fred", freq = "quarter", start = 2024)
#' get_series_src("v2062810", "statcan")
#' get_series_src("FXMUSDCAD", "boc", start = 2017)
#' get_series_src(
#'   "0004052037", "estat",
#'   filters = list(cdTab = "1", cdCat01 = "0001", cdArea = "00000")
#' )
#' get_series_src(
#'   "WTIPUUS", "eia",
#'   filters = list(route = "steo", frequency = "monthly")
#' )
get_series_src <- function(
  id,
  source = c("fred", "statcan", "boc", "estat", "eia"),
  freq = NULL,
  start = NULL,
  filters = list(),
  format = c("wide", "long", "xts")
) {
  source <- match.arg(source)
  format <- match.arg(format)
  start <- if (is.null(start)) as.Date("1940-01-01") else to_ymd(start)
  if (!is.null(freq) && !freq %in% c("month", "quarter")) {
    rlang::abort("`freq` must be NULL, \"month\" or \"quarter\".")
  }
  reader <- switch(
    source,
    fred = read_src_fred,
    statcan = read_src_statcan,
    boc = read_src_boc,
    estat = read_src_estat,
    eia = read_src_eia
  )
  long <- purrr::map(id, function(i) {
    x <- reader(i, start = start, filters = filters)
    if (!is.null(freq)) {
      x <- period_mean(x, freq, start)
    }
    dplyr::mutate(x, id = i, .before = 1)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$id, .data$time)
  switch(
    format,
    long = long,
    wide = tsbox::ts_wide(long),
    xts = tsbox::ts_xts(long)
  )
}


#' Values averaged to months or quarters (get_series_src(freq = ))
#'
#' @param x tibble of time and value, one series.
#' @param unit "month" or "quarter".
#' @param start the first date requested.
#' @return tibble of time (first day of each period) and value, the periods
#'   described in get_series_src()'s details.
#' @noRd
period_mean <- function(x, unit, start) {
  x <- dplyr::filter(x, !is.na(.data$value))
  if (nrow(x) == 0) {
    return(x)
  }
  n <- c(month = 1, quarter = 3)[[unit]]
  # months between observations: 0 for daily or weekly data
  step <- if (nrow(x) > 1) round(stats::median(as.numeric(diff(x$time))) / 30.44) else 0
  if (step > n) {
    rlang::abort(paste0("`freq = \"", unit, "\"` is not lower than the source's frequency."))
  }
  out <- x %>%
    dplyr::mutate(period = lubridate::floor_date(.data$time, unit)) %>%
    dplyr::summarise(
      value = mean(.data$value),
      first = min(.data$time),
      last = max(.data$time),
      .by = "period"
    ) %>%
    dplyr::arrange(.data$period)
  keep <- out$period < lubridate::floor_date(Sys.Date(), unit) & out$period >= start
  if (step >= 1) {
    # monthly data: the series' first and last periods need their first and
    # last months
    i_first <- 1
    i_last <- nrow(out)
    keep[i_first] <- keep[i_first] && out$first[i_first] == out$period[i_first]
    keep[i_last] <- keep[i_last] &&
      out$last[i_last] >= lubridate::add_with_rollback(
        out$period[i_last], months(n - step)
      )
  }
  tibble::tibble(time = out$period[keep], value = out$value[keep])
}


#' @noRd
read_src_fred <- function(id, start, filters) {
  url <- httr::modify_url(
    "https://api.stlouisfed.org/fred/series/observations",
    query = c(
      list(
        series_id = id,
        api_key = api_key("fred"),
        file_type = "json",
        observation_start = format(start)
      ),
      filters
    )
  )
  obs <- read_json_retry(url)$observations
  tibble::tibble(
    time = as.Date(obs$date),
    # FRED writes missing values as "."
    value = suppressWarnings(as.numeric(obs$value))
  )
}


#' @noRd
read_src_statcan <- function(id, start, filters) {
  url <- stringr::str_c(
    "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorByReferencePeriodRange",
    "?vectorIds=%22", stringr::str_remove(id, "^v"), "%22",
    "&startRefPeriod=", format(start),
    "&endReferencePeriod=", format(Sys.Date() + 400)
  )
  res <- read_json_retry(url)
  if (length(res) == 0) {
    rlang::abort(stringr::str_c(
      "StatCan ", id, ": empty response; the service is down for ",
      "maintenance overnight (Eastern time), try later."
    ))
  }
  if (res$status[1] != "SUCCESS") {
    rlang::abort(stringr::str_c("StatCan ", id, ": ", res$status[1]))
  }
  pts <- res$object$vectorDataPoint[[1]]
  tibble::tibble(time = as.Date(pts$refPer), value = as.numeric(pts$value))
}


#' @noRd
read_src_boc <- function(id, start, filters) {
  url <- stringr::str_c(
    "https://www.bankofcanada.ca/valet/observations/", id,
    "/json?start_date=", format(start)
  )
  obs <- read_json_retry(url)$observations
  tibble::tibble(time = as.Date(obs$d), value = as.numeric(obs[[id]]$v))
}


#' @noRd
read_src_estat <- function(id, start, filters) {
  query <- stringr::str_c("&", names(filters), "=", unlist(filters), collapse = "")
  out <- list()
  pos <- 1
  repeat {
    url <- stringr::str_c(
      "https://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=",
      api_key("estat"), "&statsDataId=", id, query,
      "&metaGetFlg=N&startPosition=", pos
    )
    res <- read_json_retry(url)$GET_STATS_DATA
    if (res$RESULT$STATUS != 0) {
      rlang::abort(stringr::str_c("e-Stat ", id, ": ", res$RESULT$ERROR_MSG))
    }
    out[[length(out) + 1]] <- tibble::as_tibble(res$STATISTICAL_DATA$DATA_INF$VALUE)
    pos <- res$STATISTICAL_DATA$RESULT_INF$NEXT_KEY
    if (is.null(pos)) break
  }
  # time codes: yyyy00mmmm, monthly when mm != 00, annual yyyy000000
  dat <- dplyr::bind_rows(out) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_sub(.data$`@time`, 1, 4)),
      month = as.integer(stringr::str_sub(.data$`@time`, 7, 8)),
      kind = stringr::str_sub(.data$`@time`, 5, 6)
    ) %>%
    dplyr::filter(.data$kind == "00")
  monthly <- dplyr::filter(dat, .data$month != 0)
  dat <- if (nrow(monthly) > 0) monthly else dplyr::mutate(dat, month = 1L)
  x <- dat %>%
    dplyr::transmute(
      time = lubridate::make_date(.data$year, .data$month),
      value = suppressWarnings(as.numeric(.data$`$`))
    ) %>%
    dplyr::filter(!is.na(.data$value), .data$time >= start) %>%
    dplyr::arrange(.data$time)
  if (anyDuplicated(x$time)) {
    rlang::abort(stringr::str_c(
      "e-Stat ", id, ": the filters leave several values per date; ",
      "add category codes (cdCat01 = ..., cdArea = ...) to pick one."
    ))
  }
  x
}


#' @noRd
read_src_eia <- function(id, start, filters) {
  if (is.null(filters$route)) {
    rlang::abort("EIA needs `filters = list(route = ...)`, e.g. \"steo\" or \"aeo/2025\".")
  }
  route <- stringr::str_remove(filters$route, "/?(data/?)?$")
  rest <- filters[setdiff(names(filters), "route")]
  # anything other than the API's own parameters is a facet
  own <- c("frequency", "start", "end", "sort", "offset", "length")
  facets <- rest[setdiff(names(rest), own)]
  query <- c(
    list(api_key = api_key("eia"), "data[0]" = "value", length = 5000),
    rest[intersect(names(rest), own)],
    stats::setNames(facets, stringr::str_c("facets[", names(facets), "][]")),
    list("facets[seriesId][]" = id)
  )
  url <- httr::modify_url(
    stringr::str_c("https://api.eia.gov/v2/", route, "/data/"),
    query = query
  )
  dat <- tibble::as_tibble(read_json_retry(url)$response$data)
  if (nrow(dat) == 0) {
    rlang::abort(stringr::str_c("EIA ", id, ": no data for these filters."))
  }
  if (anyDuplicated(dat$period)) {
    rlang::abort(stringr::str_c(
      "EIA ", id, ": several values per date; add a facet (e.g. scenario = ...)."
    ))
  }
  period <- as.character(dat$period)
  time <- dplyr::case_when(
    stringr::str_detect(period, "^\\d{4}$") ~ stringr::str_c(period, "-01-01"),
    stringr::str_detect(period, "^\\d{4}-\\d{2}$") ~ stringr::str_c(period, "-01"),
    TRUE ~ period
  )
  tibble::tibble(time = as.Date(time), value = as.numeric(dat$value)) %>%
    dplyr::filter(.data$time >= start) %>%
    dplyr::arrange(.data$time)
}


#' Download a table, save it, and read it back, or read the saved copy
#'
#' Scripts download their data on a full run and reread it on a rerun, so the
#' rerun works offline and gives the same numbers. `fetch_cached()` does both.
#' `fetch` downloads the table; `path` is where the copy is kept.
#'
#' @param path CSV file that holds the saved copy. Its folder is created if
#'   needed.
#' @param fetch function with no arguments that downloads the table and
#'   returns a data frame, e.g. `function() get_series_exp(324)`.
#' @param mode
#'   * "refresh" (default): download, save to `path`, and read the saved file
#'     back.
#'   * "missing": download only when `path` does not exist yet; otherwise
#'     read it. For large files that change rarely.
#'   * "offline": only read `path`; stop if it does not exist.
#' @param ... passed to [readr::read_csv()], e.g. `col_types`.
#'
#' @return the table, as read from `path`
#' @export
#'
#' @details The table is always read back from the file, also right after a
#'   download, so a full run and a later offline rerun see identical values.
#'   readr writes numbers so that they read back exactly.
#'
#' @examplesIf interactive()
#' from_disk <- FALSE
#' hist <- fetch_cached(
#'   "data/raw/324.csv",
#'   function() get_series_exp(324, rename = "no"),
#'   mode = if (from_disk) "offline" else "refresh"
#' )
fetch_cached <- function(path, fetch, mode = c("refresh", "missing", "offline"), ...) {
  mode <- match.arg(mode)
  saved <- file.exists(path)
  if (mode == "offline" && !saved) {
    rlang::abort(c(
      paste0(path, " is not saved yet."),
      i = 'Run once with mode = "refresh" or "missing" to download it.'
    ))
  }
  if (mode == "refresh" || (mode == "missing" && !saved)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(fetch(), path)
  }
  readr::read_csv(path, show_col_types = FALSE, ...)
}
