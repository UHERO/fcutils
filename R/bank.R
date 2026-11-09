#' Bank start and end, from the options `fcutils.bnk_start` and
#' `fcutils.bnk_end`
#'
#' @return a Date; an error if the option is not set
#' @noRd
bank_start <- function() {
  bank_option("fcutils.bnk_start")
}

#' @rdname bank_start
#' @noRd
bank_end <- function() {
  bank_option("fcutils.bnk_end")
}

bank_option <- function(name) {
  x <- getOption(name)
  if (is.null(x)) {
    stop(
      "No bank set (", name, "). Set it once, for example in setup.R:\n",
      "  options(fcutils.bnk_start = as.Date(\"1970-01-01\"), ",
      "fcutils.bnk_end = as.Date(\"2070-12-31\"))",
      call. = FALSE
    )
  }
  to_ymd(x)
}


#' Create xts and fill with values
#'
#' @param start date of series start (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param end date of series end (character: "yyyy-mm-dd", "yyyyqq", "yyyy")
#' @param per periodicity of series (character: "year" - default)
#'   if start is written as a quarter or a month (see `to_ymd()`), per is set
#'   to "quarter" or "month"
#' @param val values to fill in (numeric scalar, vector, or tibble)
#'
#' @return an xts series
#' @export
#'
#' @details when end is missing, but val is a vector of more than one element,
#' the end date is automatically determined by the length of the val vector.
#' if end is missing and val is a scalar, the end date is set to the bank end,
#' `getOption("fcutils.bnk_end")`. The default start is the bank start,
#' `getOption("fcutils.bnk_start")`. Set both once per session, e.g. in
#' setup.R: `options(fcutils.bnk_start = as.Date("1970-01-01"),
#' fcutils.bnk_end = as.Date("2070-12-31"))`.
#' if end is missing the remaining arguments have to be named. if val is a tibble,
#' the end date is automatically determined by the number of rows in the tibble.
#'
#' @examples
#' options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))
#' make_xts()
#' make_xts(val = 0, per = "m")
#' make_xts(start = 20100101, per = "quarter", val = 0)
#' make_xts(start = 2010.1, per = "q", val = 1:10)
#' make_xts(2010.1, val = 1:10) # automatically set per = "quarter"
#' make_xts(start = "2010-01-01", per = "m", val = 0)
#' make_xts(start = 201001, per = "q",
#'          val = tibble::tibble(E_NF_HON = c(1:10), ECT_HI = c(11:20)))
make_xts <- function(
  start = bank_start(),
  end = NULL,
  per = "year",
  val = NA_real_
) {
  start_unit <- parse_period(start)$unit[1]
  if (start_unit %in% c("quarter", "month")) {
    per <- start_unit
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
      time = seq.Date(from = start, to = bank_end(), by = per),
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


#' Create an add-factor
#'
#' An add-factor is a series added to a forecast to shift it for a while.
#' `addf()` returns one that is zero over the bank except where you set it:
#' along a straight-line ramp, along several ramps that add up, or through
#' points given as formulas.
#'
#' @param start date the ramp starts, in any form `to_ymd()` reads
#'   ("2026-07-01", "2026Q3", 2026.3, 2026), or a data frame of ramps with
#'   columns start, end, from and to, one ramp per row
#' @param end date the ramp ends
#' @param from value at `start`
#' @param to value at `end`
#' @param ser_name name of the series
#' @param per frequency: "year" (default), "quarter" or "month". A start
#'   written as a quarter ("2026Q3", 2026.3) or a month ("2026M07") sets it.
#' @param ... with formulas: further points (see details)
#' @param interp with formulas: how to join the points, "linear" (default)
#'   or "stine", the Stineman spline, which eases in and out of each point
#'   without overshooting. Must be named.
#'
#' @return one xts series over the bank, from
#'   `getOption("fcutils.bnk_start")` to `getOption("fcutils.bnk_end")` (see
#'   `make_xts()`)
#' @export
#'
#' @details A ramp runs in equal steps from `from` at `start` to `to` at
#' `end`. Before `start` and after `end` the add-factor is zero, so a ramp
#' that ends away from zero drops back to zero in the next period. With no
#' arguments, `addf()` gives zeros over the bank.
#'
#' Given a data frame of ramps, `addf()` builds one add-factor per row and
#' returns their sum, so overlapping ramps add up. A data frame with no rows
#' gives zeros over the bank. A date column must be all numbers or all
#' strings.
#'
#' Given formulas, `addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2030.4 ~ 0)`, the
#' add-factor runs through the points in straight lines, one equal step per
#' period, and is zero before the first point and after the last. Each
#' formula has a date on the left and a value on the right. The frequency
#' follows the first date (2026.3 is a quarter), unless `per` is given by
#' name; `ser_name` and `per` must be named. Decimal dates are quarters, so
#' monthly points need strings (`"2026M10" ~ 1`).
#'
#' With `interp = "stine"` the points are joined by a smooth curve instead.
#' The spline treats the zeros before the first point and after the last as
#' known, so an add-factor that starts and ends at zero leaves and returns to
#' zero gradually. A ramp joins only two points, so `interp` applies to
#' formulas, not to start, end, from and to or a table of ramps.
#'
#' @examples
#' options(
#'   fcutils.bnk_start = as.Date("2020-01-01"),
#'   fcutils.bnk_end = as.Date("2035-12-31")
#' )
#' # one ramp: from 0 in 2026Q3 to -0.3 in 2027Q4, zero outside
#' addf("2026Q3", "2027Q4", 0, -0.3)[p(2026.2, 2028.1)]
#' # a ramp that ends away from zero drops back to zero after its end
#' addf(2026.3, 2027.4, 1, 2, ser_name = "ECT_HI")[p(2027.3, 2028.2)]
#' # annual and monthly
#' addf(2026, 2028, 0, 1)[p(2025, 2029)]
#' addf("2026M01", "2026M06", 0, 0.5)[p("2025M12", "2026M07")]
#' # zeros over the bank
#' addf(per = "quarter")[p(2026.1, 2026.4)]
#'
#' # several ramps at once: down to -0.3 by 2027Q4, then back to 0 by 2030Q4
#' ramps <- tibble::tribble(
#'   ~start,   ~end,     ~from, ~to,
#'   "2026Q3", "2027Q4", 0,     -0.3,
#'   "2028Q1", "2030Q4", -0.3,  0
#' )
#' ramps_adj <- addf(ramps)
#' round(ramps_adj[p(2026.2, 2031.1)], 3)
#' # the same as points, with decimal or quoted dates
#' pts_adj <- addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2028.1 ~ -0.3, 2030.4 ~ 0)
#' all.equal(pts_adj, ramps_adj)
#' addf("2026Q3" ~ 0, "2027Q4" ~ -0.3, "2028Q1" ~ -0.3, "2030Q4" ~ 0)[p(2027.3, 2028.2)]
#' # a smooth curve through the same points
#' smooth_adj <- addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2028.1 ~ -0.3, 2030.4 ~ 0,
#'                    interp = "stine")
#' round(tsbox::ts_c(linear = pts_adj, stine = smooth_adj)[p(2026.2, 2031.1)], 3)
addf <- function(
  start = bank_start(),
  end = bank_end(),
  from = 0,
  to = 0,
  ser_name = "value",
  per = "year",
  ...,
  interp = c("linear", "stine")
) {
  interp <- match.arg(interp)
  if (inherits(start, "formula")) {
    # formulas fill the positional slots first; collect them all
    pts <- list(start)
    if (!missing(end)) pts <- c(pts, list(end))
    if (!missing(from)) pts <- c(pts, list(from))
    if (!missing(to)) pts <- c(pts, list(to))
    if (!missing(ser_name) && inherits(ser_name, "formula")) {
      pts <- c(pts, list(ser_name))
      ser_name <- "value"
    }
    per_given <- !missing(per)
    if (per_given && inherits(per, "formula")) {
      pts <- c(pts, list(per))
      per_given <- FALSE
    }
    pts <- c(pts, list(...))
    return(addf_points(pts, ser_name, if (per_given) per else NULL, interp))
  }
  if (interp != "linear") {
    stop(
      "addf(): interp applies to points given as formulas (2026.3 ~ 0), not to ramps",
      call. = FALSE
    )
  }
  slots <- list(end, from, to, ser_name, per)
  if (length(list(...)) > 0 || any(vapply(slots, inherits, logical(1), "formula"))) {
    stop(
      "addf(): formulas (2026.3 ~ 0) cannot be mixed with start, end, from and to",
      call. = FALSE
    )
  }
  if (is.data.frame(start)) {
    ramps <- start
    missing_cols <- setdiff(c("start", "end", "from", "to"), names(ramps))
    if (length(missing_cols) > 0) {
      stop(
        "addf(): the ramp table lacks the column(s) ",
        stringr::str_flatten(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    if (nrow(ramps) == 0) {
      return(addf(ser_name = ser_name, per = per))
    }
    parts <- purrr::pmap(
      ramps[c("start", "end", "from", "to")],
      function(start, end, from, to) {
        addf(start, end, from, to, ser_name = ser_name, per = per)
      }
    )
    return(Reduce(`+`, parts))
  }
  start_unit <- parse_period(start)$unit[1]
  if (identical(start_unit, "month")) {
    per <- "month"
  }
  if (identical(start_unit, "quarter") || per == "quarter") {
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


#' addf() from points given as formulas
#' @noRd
addf_points <- function(pts, ser_name, per = NULL, interp = "linear") {
  fp <- formula_points(pts, "addf")
  if (is.null(per)) {
    per <- fp$units[1]
    if (identical(per, "day")) {
      stop("addf(): with daily dates, give per by name", call. = FALSE)
    }
  }
  if (per == "month" && any(fp$decimal)) {
    stop(
      "addf(): monthly points are dated as decimal years ",
      "(2026.1 reads as 2026Q1). Write months as \"2026M10\".",
      call. = FALSE
    )
  }
  ord <- order(fp$dates)
  dates <- fp$dates[ord]
  values <- fp$values[ord]
  out <- make_xts(val = 0, per = per)
  pos <- match(dates, zoo::index(out))
  if (anyNA(pos)) {
    stop(
      "addf(): ", stringr::str_flatten(format(dates[is.na(pos)]), collapse = ", "),
      " is not the start of a ", per, " within the bank",
      call. = FALSE
    )
  }
  span <- seq(min(pos), max(pos))
  if (interp == "linear") {
    out[span] <- stats::approx(pos, values, xout = span)$y
  } else {
    # zeros outside the points are known, so the curve eases off and back to zero
    out[span] <- NA
    out[pos] <- values
    out <- stine_fill(out)
  }
  colnames(out) <- ser_name
  out
}
