#' Year to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return year to date average, if FALSE, return year to date sum
#'
#' @return object of the same type as the input containing year to date sum or average
#'
#' @details An NA makes the rest of its period NA.
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   ytd_agg() |>
#'   tail()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   ytd_agg(avg = FALSE) |>
#'   tsbox::ts_plot()
ytd_agg <- function(x, avg = TRUE) {
  to_date(x, "year", avg = avg)
}


#' Year to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing year to date growth rate
#'
#' @details The growth rate is the percent change of the to-date average from
#' the same date a year earlier. An NA makes the rest of its period NA.
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
  to_date(x, "year", growth = "pcy")
}


#' Fiscal year to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return fiscal year to date average, if FALSE, return fiscal year to date sum
#'
#' @return object of the same type as the input containing fiscal year to date sum or average
#' @export
#'
#' @details Works as ytd_agg(), with the fiscal year starting in July. An NA
#' makes the rest of its fiscal year NA.
#'
#' @examples
#' monthly_data_example |>
#'   fytd_agg() |>
#'   tail()
#' quarterly_data_example |>
#'   fytd_agg() |>
#'   tail()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   fytd_agg(avg = FALSE) |>
#'   tsbox::ts_plot()
fytd_agg <- function(x, avg = TRUE) {
  to_date(x, "year", avg = avg, fiscal = TRUE)
}


#' Fiscal year to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing fiscal year to date growth rate
#' @export
#'
#' @details Works as ytd_gr(), with the fiscal year starting in July: the
#' percent change of the fiscal-year-to-date average from the same date a year
#' earlier. An NA makes the rest of its fiscal year NA.
#'
#' @examples
#' quarterly_data_example |>
#'   fytd_gr() |>
#'   tail()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   tsbox::ts_xts() |>
#'   fytd_gr() |>
#'   tail()
fytd_gr <- function(x) {
  to_date(x, "year", fiscal = TRUE, growth = "pcy")
}


#' Month to date sum or average
#'
#' @param x a ts-boxable object
#' @param avg if TRUE (default), return month to date average, if FALSE, return month to date sum
#'
#' @return object of the same type as the input containing month to date sum or average
#'
#' @details An NA makes the rest of its period NA.
#' @export
#'
#' @examples
#' daily_data_example |>
#'   mtd_agg() |>
#'   tail()
mtd_agg <- function(x, avg = TRUE) {
  to_date(x, "month", avg = avg)
}


#' Month to date growth rate
#'
#' @param x a ts-boxable object
#'
#' @return object of the same type as the input containing month to date growth rate
#' @export
#'
#' @details The growth rate is the percent change of the month-to-date average
#' from the same date a year earlier. An NA makes the rest of its month NA.
#'
#' @examples
#' daily_data_example |>
#'   mtd_gr() |>
#'   tail()
mtd_gr <- function(x) {
  to_date(x, "month", growth = "pcy")
}


#' Period to date sum or average
#'
#' @param x a ts-boxable object
#' @param per period over which the sum or average of a higher frequency series in x is calculated
#'   (this is the unit of time at which the aggregation is performed:
#'   for ytd per = "year" (default), for mtd per = "month")
#' @param avg if TRUE (default), return period to date average, if FALSE, return period to date sum
#'
#' @return object of the same type as the input containing period to date sum or average
#' @export
#'
#' @details `per` takes any unit `lubridate::floor_date()` does, such as
#' "week" or "day". Weeks start on Sunday. An NA makes the rest of its period
#' NA.
#'
#' @examples
#' daily_data_example |>
#'   ptd_agg("week")
#' test <- daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   ptd_agg("week")
#' tsbox::`%ts/%`(test, tsbox::ts_lag(test, "4 weeks")) |>
#'   tsbox::`%ts-%`(1) |>
#'   tsbox::`%ts*%`(100) |>
#'   tail()
ptd_agg <- function(x, per = "year", avg = TRUE) {
  to_date(x, per, avg = avg)
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
#' @details The growth rate is the percent change of the period-to-date average
#' from the same date `lag_length` earlier. Weeks start on Sunday. An NA makes
#' the rest of its period NA.
#'
#' @examples
#' monthly_data_example |>
#'   ptd_gr() |>
#'   tail()
#' monthly_data_example |>
#'   dplyr::select("time", "VAPNS_HI") |>
#'   ptd_gr(per = "month", lag_length = "3 years") |>
#'   tail()
#' # with per = "week", compare with 52 weeks back so the same weekdays line
#' # up; "1 year" back lands on a different weekday
#' daily_data_example |>
#'   ptd_gr("week", "52 weeks") |>
#'   tail()
#' # and use lag_length = "364 days" with daily data
#' daily_data_example |>
#'   ptd_gr("day", "364 days") |>
#'   tail()
#' daily_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VAPNS_HI") |>
#'   ptd_gr("week", "4 weeks") %>%
#'   tail()
ptd_gr <- function(x, per = "year", lag_length = "1 year") {
  to_date(x, per, growth = lag_length)
}


#' Period-to-date averages or sums, and their growth (used by the to-date
#' functions)
#'
#' @param per period within which values accumulate ("year", "quarter",
#'   "month", ...)
#' @param avg TRUE for the running mean, FALSE for the running sum
#' @param fiscal TRUE for fiscal years (July to June): the data are shifted by
#'   six months before grouping and back after
#' @param growth NULL (none), "pcy" (tsbox::ts_pcy()), or a lag such as
#'   "1 year": % change on the value that far back
#' @noRd
to_date <- function(x, per, avg = TRUE, fiscal = FALSE, growth = NULL) {
  x_mod <- conv_long(x, ser_info = TRUE)
  out <- x_mod
  if (fiscal) {
    out <- tsbox::ts_lag(out, "6 months")
  }
  out <- out %>%
    dplyr::mutate(time_per = lubridate::floor_date(.data$time, per)) %>%
    dplyr::group_by(.data$id, .data$time_per) %>%
    dplyr::mutate(
      value = if (avg) dplyr::cummean(.data$value) else cumsum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"time_per")
  if (fiscal) {
    out <- tsbox::ts_lag(out, "-6 months")
  }
  # pcy() and lag_ratio() give tsbox::ts_pcy() and
  # out %ts/% tsbox::ts_lag(out, growth), faster when no date is missing
  if (identical(growth, "pcy")) {
    out <- pcy(out)
  } else if (!is.null(growth)) {
    out <- if (is.na(lag_periods(out, growth))) {
      (out %ts/% tsbox::ts_lag(out, growth) %ts-% 1) %ts*% 100
    } else {
      dplyr::mutate(lag_ratio(out, growth), value = (.data$value - 1) * 100)
    }
  }
  reclass(out, x_mod, x)
}
