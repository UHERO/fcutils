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

    # # univariate data requires special treatment
    # {
    #   if (length(attr(x_mod, "ser_names")) == 1) {
    #     tsbox::ts_tbl(.) %>%
    #       tsbox::ts_long() %>%
    #       dplyr::mutate(id = attr(x_mod, "ser_names"))
    #   } else {
    #     tsbox::ts_tbl(.)
    #   }
    # }
    tsbox::ts_tbl()

  # reclass the output to match the input
  ans <- reclass(x_mod_span, x_mod, x)

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

  # x_mod %>%
  #   tsbox::ts_summary() %>%
  #   dplyr::pull(.data$start)
  obs_range(x_mod)$start
}


#' First and last date with a value, by series (as in `tsbox::ts_summary()`)
#'
#' Input with a series of fewer than two values goes to ts_summary(), which
#' stops on it, so such input fails as it always has.
#' @param x_mod long tibble from conv_long()
#' @noRd
obs_range <- function(x_mod) {
  if (!enough_values(x_mod)) {
    return(tsbox::ts_summary(x_mod))
  }
  x_mod %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::summarise(
      start = min(.data$time),
      end = max(.data$time),
      .by = "id"
    )
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

  # ts_summary() is slow; it is only needed for the period length
  x_mod_summary <- if (last_day) {
    tsbox::ts_summary(x_mod)
  } else {
    obs_range(x_mod)
  }

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
#' @param dat1 date of period start (any form read by `to_ymd()`)
#' @param dat2 date of period end (any form read by `to_ymd()`)
#'
#' @return string containing date range, for subsetting xts objects
#' @export
#'
#' @details The range runs from the first day of dat1 to the last day of
#' dat2: a year, quarter or month given as dat2 is included in full, so
#' `p(2019.4, 2026)` ends on 2026-12-31. A day given as dat2 is kept as is.
#'
#' @examples
#' p("2010-01-01", "2020-01-01")
#' p(20100101, 20200101)
#' p(2010.1, 2020.4)
#' p(,2020.4)
#' p("2010Q1", "2020Q4")
#' p(2010, 2020)
#' p("2010M1", "2010M6")
p <- function(dat1 = "", dat2 = "") {
  if (nchar(dat1) != 0) {
    dat1 <- to_ymd(dat1)
  }
  if (nchar(dat2) != 0) {
    dat2 <- period_end(dat2)
  }
  stringr::str_c(dat1, dat2, sep = "/")
}


#' Last day of the period a date names
#'
#' @param x dates as accepted by `to_ymd()`
#'
#' @return Dates: the last day of each year, quarter or month given; days
#'   are returned unchanged
#' @noRd
period_end <- function(x) {
  pp <- parse_period(x)
  out <- pp$date
  step <- c(year = 12L, quarter = 3L, month = 1L)[pp$unit]
  i <- !is.na(step)
  out[i] <- lubridate::add_with_rollback(out[i], months(step[i])) - 1
  out
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


#' Read dates written as years, quarters, months or days
#'
#' @param x dates as numbers, strings or Dates (see details)
#'
#' @return Dates, each the first day of the period given
#' @export
#'
#' @details Accepted forms, as numbers or strings:
#' * year: `2019`
#' * quarter: `2019.4`, `"2019Q4"`, `"2019q4"`, `201904` (yyyyqq, quarters
#'   01 to 04)
#' * month (strings only): `"2019M10"`, `"2019m3"`, `"2019-10"`
#' * day: `"2019-10-15"`, `20191015`, a Date
#'
#' All of `2019.4`, `"2019Q4"` and `201904` give 2019-10-01. Months cannot be
#' numbers, because `2019.10` and `2019.1` are the same number. Anything else
#' stops with an error naming the value. `NA` and `""` give `NA`.
#'
#' @examples
#' to_ymd(c(2020, 2020.2, "2020Q2", 202002, "2020M5", "2020-05", "2020-05-15"))
to_ymd <- function(x) {
  parse_period(x)$date
}


#' Parse dates and keep the period each one names
#'
#' @param x dates as accepted by `to_ymd()`
#'
#' @return list with `date` (first day of each period) and `unit` ("year",
#'   "quarter", "month", "day" or NA)
#' @noRd
parse_period <- function(x) {
  if (inherits(x, "Date")) {
    return(list(date = x, unit = rep("day", length(x))))
  }
  if (inherits(x, "POSIXt")) {
    return(list(date = as.Date(x), unit = rep("day", length(x))))
  }
  s <- trimws(as.character(x))
  n <- length(s)
  date <- rep(as.Date(NA), n)
  unit <- rep(NA_character_, n)
  ok <- is.na(s) | s == ""

  # each form: pattern, unit, and how to build the date from the matches
  num <- function(m, i) as.integer(m[, i])
  forms <- list(
    list("^(\\d{4})$", "year", function(m) lubridate::make_date(num(m, 2))),
    list("^(\\d{4})[.Qq]([1-4])$", "quarter",
         function(m) lubridate::make_date(num(m, 2), 3 * num(m, 3) - 2)),
    list("^(\\d{4})0([1-4])$", "quarter",
         function(m) lubridate::make_date(num(m, 2), 3 * num(m, 3) - 2)),
    list("^(\\d{4})[Mm-](\\d{1,2})$", "month",
         function(m) lubridate::make_date(num(m, 2), num(m, 3))),
    list("^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})$", "day",
         function(m) lubridate::make_date(num(m, 2), num(m, 3), num(m, 4))),
    list("^(\\d{4})(\\d{2})(\\d{2})$", "day",
         function(m) lubridate::make_date(num(m, 2), num(m, 3), num(m, 4)))
  )
  for (f in forms) {
    todo <- !ok & stringr::str_detect(s, f[[1]])
    if (!any(todo)) next
    m <- stringr::str_match(s[todo], f[[1]])
    d <- suppressWarnings(f[[3]](m))
    # month 13 or day 32 give NA: leave them for the error below
    good <- !is.na(d)
    idx <- which(todo)[good]
    date[idx] <- d[good]
    unit[idx] <- f[[2]]
    ok[idx] <- TRUE
  }
  if (!all(ok)) {
    stop(
      "to_ymd() cannot read ",
      stringr::str_flatten(unique(s[!ok]), collapse = ", "),
      ". Use a year (2019), a quarter (2019.4, \"2019Q4\", 201904), ",
      "a month (\"2019M10\", \"2019-10\") or a day (\"2019-10-15\").",
      call. = FALSE
    )
  }
  list(date = date, unit = unit)
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


#' Calculate number of months between two dates yyyyMm or yyyy-mm-dd
#'
#' @param dat1 date of period start (string: yyyyMm or yyyy-mm-dd; see `to_ymd()`)
#' @param dat2 date of period end (string: yyyyMm or yyyy-mm-dd; see `to_ymd()`)
#'
#' @return numeric length of date range in months
#' @export
#'
#' @details The endpoints are included in the result so subtract one for time difference.
#' Also, the result is rounded down so partial months are not counted. See examples.
#'
#' @examples
#' nmons("2010M1", "2010M2")
#' nmons("2010M1", "2010M4")
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
