# Fast versions of tsbox operations for long tables whose dates are complete
#
# tsbox's lag, arithmetic and frequency conversion check and convert their
# input on every call, which is slow on large banks. When every series is a
# sorted run of complete dates (dates_complete()), a lag is the value a fixed
# number of rows earlier in the same series, and a period's aggregate is a
# grouped mean or sum. Each function here gives the same rows and values as
# the tsbox call it replaces, and falls back to that call for anything else.


#' Months between consecutive dates, if every series shares one step
#'
#' @param x long tibble with columns id, time, value
#' @return 1, 3, 12, ... or NA if the dates are not complete, not on the first
#'   of the month, or the series differ in step
#' @noRd
month_step <- function(x) {
  if (!dates_complete(x) || !identical(names(x), c("id", "time", "value"))) {
    return(NA_integer_)
  }
  if (!all(lubridate::mday(x$time) == 1L)) {
    return(NA_integer_)
  }
  months <- lubridate::year(x$time) * 12L + lubridate::month(x$time)
  same_id <- x$id[-1] == x$id[-length(x$id)]
  steps <- unique(diff(months)[same_id])
  if (length(steps) != 1L || steps < 1L) {
    return(NA_integer_)
  }
  as.integer(steps)
}


#' `x %ts/% tsbox::ts_lag(x, k)`: value over its value k periods earlier
#'
#' Like tsbox, drops rows whose result is NA or NaN, and keeps Inf.
#'
#' @param x long tibble with columns id, time, value
#' @param k lag: a number of periods, or a string such as "1 year" or
#'   "3 months"
#' @noRd
lag_ratio <- function(x, k) {
  n <- lag_periods(x, k)
  if (is.na(n)) {
    return(x %ts/% tsbox::ts_lag(x, k))
  }
  x %>%
    dplyr::mutate(
      value = .data$value / dplyr::lag(.data$value, n),
      .by = "id"
    ) %>%
    dplyr::filter(!is.na(.data$value))
}


#' `tsbox::ts_pcy(x)`: % change on the same period a year earlier
#'
#' Like tsbox, keeps every row (NA where there is no value a year earlier).
#'
#' @param x long tibble with columns id, time, value
#' @noRd
pcy <- function(x) {
  n <- lag_periods(x, "1 year")
  if (is.na(n)) {
    return(tsbox::ts_pcy(x))
  }
  dplyr::mutate(
    x,
    value = (.data$value / dplyr::lag(.data$value, n) - 1) * 100,
    .by = "id"
  )
}


#' TRUE if every series has at least two values
#'
#' tsbox stops on a series with fewer (it cannot tell the frequency); the fast
#' functions leave such input to tsbox, so it fails as it always has.
#' @noRd
enough_values <- function(x) {
  n <- tapply(!is.na(x$value), x$id, sum)
  length(n) > 0L && all(n >= 2L)
}


#' TRUE if each series has missing values only at its start and end
#'
#' ts_frequency() drops missing values and infers the frequency from the
#' dates left; with gaps inside a series that can differ from the full
#' series, so such input is left to tsbox.
#' @noRd
na_only_at_ends <- function(x) {
  all(tapply(x$value, x$id, function(v) {
    have <- which(!is.na(v))
    length(have) == 0L || all(diff(have) == 1L)
  }))
}


#' A lag in rows, or NA if it cannot be taken by position
#'
#' A number of periods works whenever the dates are complete; "n years",
#' "n quarters" or "n months" needs a step in months that divides it.
#' @noRd
lag_periods <- function(x, k) {
  if (!identical(names(x), c("id", "time", "value")) || !enough_values(x)) {
    return(NA_integer_)
  }
  if (is.numeric(k)) {
    if (length(k) != 1L || k != round(k) || k < 1L || !dates_complete(x)) {
      return(NA_integer_)
    }
    return(as.integer(k))
  }
  m <- stringr::str_match(k, "^(\\d+) (year|quarter|month)s?$")
  if (is.na(m[1, 1])) {
    return(NA_integer_)
  }
  months <- as.integer(m[1, 2]) * c(year = 12L, quarter = 3L, month = 1L)[[m[1, 3]]]
  step <- month_step(x)
  if (is.na(step) || months %% step != 0L) {
    return(NA_integer_)
  }
  months %/% step
}


#' `tsbox::ts_frequency(x, to, aggregate, na.rm)` for means and sums
#'
#' Like tsbox: with `na.rm = FALSE` a period counts only if all its months or
#' quarters are present and none is NA; with `na.rm = TRUE` any values present
#' count, and a period whose result is NA or NaN is dropped (a sum of NAs is 0
#' and stays).
#'
#' @param x a ts-boxable object
#' @noRd
frequency_fast <- function(x, to, aggregate = "mean", na.rm = FALSE) {
  slow <- function() {
    tsbox::ts_frequency(x, to = to, aggregate = aggregate, na.rm = na.rm)
  }
  # long data frames only: tsbox treats the NA padding of wide, xts or ts
  # input as missing dates
  has_id <- identical(names(x), c("id", "time", "value"))
  if (!aggregate %in% c("mean", "sum") || !to %in% c("year", "quarter") ||
    !is.data.frame(x) || inherits(x, "data.table") ||
    !(has_id || identical(names(x), c("time", "value")))) {
    return(slow())
  }
  x_long <- if (has_id) x else dplyr::mutate(x, id = "x", .before = 1)
  if (!enough_values(x_long) || !na_only_at_ends(x_long)) {
    return(slow())
  }
  step <- month_step(x_long)
  target <- c(year = 12L, quarter = 3L)[[to]]
  if (is.na(step) || step >= target || target %% step != 0L) {
    return(slow())
  }
  per_period <- target %/% step
  fun <- if (aggregate == "mean") mean else sum
  out <- x_long %>%
    dplyr::mutate(time = lubridate::floor_date(.data$time, to)) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      value = fun(.data$value, na.rm = na.rm),
      .by = c("id", "time")
    ) %>%
    dplyr::filter(na.rm | .data$n_obs == per_period, !is.na(.data$value)) %>%
    dplyr::select(-"n_obs")
  if (!has_id) {
    out <- dplyr::select(out, -"id")
  }
  tsbox::copy_class(out, x)
}
