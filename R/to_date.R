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
  to_date(x, "year", avg = avg)
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
  to_date(x, "year", growth = "pcy")
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
  to_date(x, "year", avg = avg, fiscal = TRUE)
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
  to_date(x, "year", fiscal = TRUE, growth = "pcy")
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
  to_date(x, "month", avg = avg)
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
  to_date(x, "month", growth = "pcy")
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
#' @examples
#' monthly_data_example |>
#'   ptd_gr() |>
#'   tail()
#' monthly_data_example |>
#'   dplyr::select("time", "VAPNS_HI") |>
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
  if (identical(growth, "pcy")) {
    out <- tsbox::ts_pcy(out)
  } else if (!is.null(growth)) {
    out <- (out %ts/% tsbox::ts_lag(out, growth) %ts-% 1) %ts*% 100
  }
  reclass(out, x_mod, x)
}
