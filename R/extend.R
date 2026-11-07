#' Extend a series past its last observation
#'
#' Continues a single series from `last` to the end of the bank, in one of
#' three ways: the series runs through anchor values (`type = "level"`), its
#' growth rate runs through anchors and the level is compounded
#' (`type = "growth"`), or it continues with the growth of another series
#' (`type = "chain"`).
#'
#' @param x a ts-boxable object holding one series (quarterly, monthly or
#'   annual)
#' @param ... anchors as formulas, date on the left and value on the right:
#'   `2026.4 ~ 4.4, 2028.4 ~ 4.0`. The right side is evaluated, so
#'   `2027.4 ~ target` works. An alternative to `at` and `val`.
#' @param at dates of the anchors, in any form `to_ymd()` reads
#'   (`c(2026.4, 2028.4)`, `c("2026Q4", "2028")`)
#' @param val anchor values (by name, like `at`): a numeric vector the length of `at`; a named
#'   numeric vector whose names are the dates (`c("2026Q4" = 4.3)`), with
#'   `at = NULL`; or a series (e.g. from `make_xts()`), whose observations are
#'   the anchors
#' @param path instead of anchors, a series that sets the forecast: the
#'   level (`type = "level"`), the annualized growth rate in percent
#'   (`type = "growth"`), or a level whose growth continues x
#'   (`type = "chain"`)
#' @param type "level", "growth" or "chain" (see details)
#' @param interp interpolation between anchors: "linear" or "stine" (the
#'   Stineman spline: smooth turns without overshooting)
#' @param last last date that counts as history (default: the last
#'   observation of x). Values of x after `last` are replaced.
#' @param hold_last if TRUE (default), the last anchor is held to the end of
#'   the bank. If FALSE, the interpolation itself runs to the last anchor and
#'   its value is then carried forward.
#' @param join for `type = "chain"`: "ratio" (default) uses the growth of
#'   `path` as it is; "shift" first moves `path` by a constant so that it
#'   meets x at `last`, then uses the growth of the shifted path
#' @param smooth NULL, or a list with `span` and optionally `from` and `to`:
#'   smooth the forecast with loess before compounding (the period growth
#'   rate for "growth" and "chain", the level for "level"). `from` defaults
#'   to the first period after `last`.
#'
#' @return an object of the same type as x, from the bank start to the bank
#'   end (`getOption("fcutils.bnk_start")`, `getOption("fcutils.bnk_end")`),
#'   empty before x's first observation
#' @export
#'
#' @details
#' * `type = "level"`: history up to `last`, then the anchors, with the gaps
#'   interpolated, including between the last history value and the first
#'   anchor. Anchors dated at or before `last` are dropped with a message,
#'   since history takes precedence.
#' * `type = "growth"`: the same applied to the annualized growth rate of x;
#'   the level is then compounded from its value at `last`. Growth is
#'   annualized at the series' frequency (4 for quarters, 12 for months).
#' * `type = "chain"`: the level of `path` after `last`, scaled to meet x.
#'
#' Linear interpolation weighs the gaps by time, the spline by position.
#'
#' Anchors can be given in four ways: formulas (`2026.4 ~ 4.4`), `at` and
#' `val`, a named vector (`val = c("2026Q4" = 4.4)`), or a series. Decimal
#' dates such as `2026.4` are quarters, so a monthly series needs month
#' strings (`"2026M10" ~ 4.4`); extend() stops if a monthly series gets
#' decimal dates.
#'
#' @examples
#' options(fcutils.bnk_start = as.Date("2015-01-01"), fcutils.bnk_end = as.Date("2035-12-31"))
#' ur <- make_xts(2024.1, val = c(3.8, 4.0, 4.2, 4.1, 4.1, 4.2, 4.3))
#' # the unemployment rate through two anchors, then held
#' extend(ur, 2026.4 ~ 4.4, 2028.4 ~ 4.0, interp = "stine")
#' extend(ur, "2026Q4" ~ 4.4, "2028Q4" ~ 4.0, interp = "stine")
#' # the same with at and val
#' extend(ur, at = c(2026.4, 2028.4), val = c(4.4, 4.0), interp = "stine")
#' # the same with the anchors as a named vector
#' extend(ur, val = c("2026Q4" = 4.4, "2028Q4" = 4.0), interp = "stine")
#' # a price level whose annualized inflation falls from 3% to 2%
#' cpi <- make_xts(2024.1, val = 100 * 1.0075^(0:6))
#' extend(cpi, at = c(2026.1, 2027.4), val = c(3, 2), type = "growth")
#' # history continued with the growth of a projection on another level
#' proj <- make_xts(2020.1, val = 50 * 1.01^(0:63))
#' extend(cpi, path = proj, type = "chain")
extend <- function(
  x,
  ...,
  at = NULL,
  val = NULL,
  path = NULL,
  type = c("level", "growth", "chain"),
  interp = c("linear", "stine"),
  last = NULL,
  hold_last = TRUE,
  join = c("ratio", "shift"),
  smooth = NULL
) {
  type <- match.arg(type)
  interp <- match.arg(interp)
  join <- match.arg(join)

  x_mod <- conv_long(x, ser_info = TRUE)
  if (length(unique(x_mod$id)) != 1) {
    stop("extend(): x must hold a single series", call. = FALSE)
  }
  ser_name <- unique(x_mod$id)
  xs <- tsbox::ts_xts(dplyr::select(x_mod, "time", "value"))
  nper <- tsbox::ts_summary(xs)$freq
  if (!isTRUE(nper %in% c(1, 4, 12))) {
    stop("extend(): x must be monthly, quarterly or annual", call. = FALSE)
  }
  per <- c("year", "quarter", "month")[match(nper, c(1, 4, 12))]
  one_period <- months(12 / nper)
  last <- if (is.null(last)) find_end(xs) else to_ymd(last)

  dots <- list(...)
  if (length(dots) > 0) {
    if (!is.null(at) || !is.null(val)) {
      stop("extend(): give anchors as formulas or as at and val, not both", call. = FALSE)
    }
    anchors <- formula_points(dots, "extend")
  } else {
    anchors <- resolve_anchors(at, val)
  }
  if (per == "month" && any(anchors$decimal)) {
    stop(
      "extend(): x is monthly, but anchors are dated as decimal years ",
      "(2026.1 reads as 2026Q1, and 2026.10 is the same number). ",
      "Write months as \"2026M10\".",
      call. = FALSE
    )
  }
  if (!is.null(anchors) && !is.null(path)) {
    stop("extend(): give anchors (at, val) or a path, not both", call. = FALSE)
  }
  if (is.null(anchors) && is.null(path)) {
    stop("extend(): give anchors (at, val) or a path", call. = FALSE)
  }
  if (type == "chain" && is.null(path)) {
    stop("extend(): type = \"chain\" needs a path", call. = FALSE)
  }
  next_period <- lubridate::add_with_rollback(last, one_period)
  smooth_from <- function() {
    if (is.null(smooth$from)) next_period else to_ymd(smooth$from)
  }

  out <- switch(
    type,
    level = {
      lev <- if (is.null(path)) {
        anchor_series(xs, anchors, last, interp, hold_last, per)
      } else {
        tsbox::ts_bind(xs[p("", last)], single_xts(path)[p(next_period, "")])
      }
      if (!is.null(smooth)) {
        lev <- smooth_window(lev, smooth$span, smooth_from(), smooth$to)
      }
      lev
    },
    growth = {
      g <- if (is.null(path)) {
        anchor_series(tsbox::ts_pca(xs), anchors, last, interp, hold_last, per)
      } else {
        single_xts(path)
      }
      compound_from(xs, pca_to_pc(g, nper), last, smooth, smooth_from())
    },
    chain = {
      pth <- single_xts(path)
      if (join == "shift") {
        pth <- pth + (value_at(xs, last) - value_at(pth, last))
      }
      g <- pca_to_pc(tsbox::ts_pca(pth), nper)
      compound_from(xs, g, last, smooth, smooth_from())
    }
  )

  # the bank, empty before x's first observation
  out <- out[paste0(find_start(xs), "/")] %>%
    tsbox::ts_bind(make_xts(per = per)) %>%
    tsbox::ts_span(bank_start(), bank_end())
  colnames(out) <- ser_name

  if (xts::is.xts(x)) {
    out
  } else {
    out_long <- tsbox::ts_tbl(out) %>%
      dplyr::mutate(id = ser_name, .before = 1)
    if (attr(x_mod, "was_wide")) {
      tsbox::ts_wide(out_long)
    } else {
      tsbox::copy_class(out_long, x)
    }
  }
}


#' Anchors as dates and values (used by `extend()`)
#'
#' @return NULL, or a list with `dates`, `values` and `decimal` (TRUE for
#'   dates given as decimal years)
#' @noRd
resolve_anchors <- function(at, val) {
  if (is.null(val)) {
    if (!is.null(at)) {
      stop("extend(): at needs val", call. = FALSE)
    }
    return(NULL)
  }
  if (!is.numeric(val) || xts::is.xts(val) || stats::is.ts(val)) {
    v <- conv_long(val) %>% tidyr::drop_na("value")
    return(list(dates = v$time, values = v$value, decimal = FALSE))
  }
  if (is.null(at)) {
    if (is.null(names(val))) {
      stop("extend(): give at, or name the values of val by date", call. = FALSE)
    }
    at <- names(val)
  }
  if (length(at) != length(val)) {
    stop("extend(): at and val differ in length", call. = FALSE)
  }
  list(
    dates = to_ymd(at),
    values = unname(as.numeric(val)),
    decimal = is_decimal(at)
  )
}


#' TRUE for dates written as decimal years (2026.4)
#' @noRd
is_decimal <- function(x) {
  if (!is.numeric(x)) {
    return(rep(FALSE, length(x)))
  }
  !is.na(x) & x != round(x) & x < 10000
}


#' Dates and values from formulas such as `2026.4 ~ 4.4`
#'
#' The left side is a date in any form `to_ymd()` reads; the right side is
#' evaluated in the formula's environment and must give one number.
#'
#' @param fs list of formulas
#' @param fn name of the calling function, for messages
#' @return list with `dates`, `values`, `units` and `decimal`
#' @noRd
formula_points <- function(fs, fn) {
  show <- function(f) paste(deparse(f), collapse = " ")
  parts <- lapply(fs, function(f) {
    if (!inherits(f, "formula")) {
      stop(
        fn, "(): unnamed arguments must be formulas such as 2026.4 ~ 4.4, not ",
        paste(format(f), collapse = " "),
        call. = FALSE
      )
    }
    if (length(f) != 3) {
      stop(fn, "(): ", show(f), " needs a date left of the ~", call. = FALSE)
    }
    lhs <- eval(f[[2]], environment(f))
    rhs <- eval(f[[3]], environment(f))
    if (length(lhs) != 1 || length(rhs) != 1 || !is.numeric(rhs)) {
      stop(fn, "(): ", show(f), " must give one date and one number", call. = FALSE)
    }
    pp <- parse_period(lhs)
    list(date = pp$date, unit = pp$unit, value = as.numeric(rhs), decimal = is_decimal(lhs))
  })
  dates <- do.call(c, lapply(parts, `[[`, "date"))
  if (anyDuplicated(dates)) {
    stop(fn, "(): two formulas give the same date: ", format(dates[duplicated(dates)][1]), call. = FALSE)
  }
  list(
    dates = dates,
    values = vapply(parts, `[[`, numeric(1), "value"),
    units = vapply(parts, `[[`, character(1), "unit"),
    decimal = vapply(parts, `[[`, logical(1), "decimal")
  )
}


#' A ts-boxable single series as xts
#' @noRd
single_xts <- function(x) {
  x_mod <- conv_long(x)
  if (length(unique(x_mod$id)) != 1) {
    stop("extend(): path must hold a single series", call. = FALSE)
  }
  tsbox::ts_xts(dplyr::select(x_mod, "time", "value"))
}


#' Value of a single xts at a date
#' @noRd
value_at <- function(x, date) {
  v <- as.numeric(x[date])
  if (length(v) != 1 || is.na(v)) {
    stop("extend(): no value at ", date, call. = FALSE)
  }
  v
}


#' History up to last, then the anchors, gaps interpolated, over the bank
#' @noRd
anchor_series <- function(x, anchors, last, interp, hold_last, per) {
  x <- tsbox::ts_bind(x, make_xts(per = per)) %>%
    tsbox::ts_span(end = bank_end())
  x[paste0(last + 1, "/")] <- NA
  dates <- anchors$dates
  values <- anchors$values
  stale <- dates <= last
  if (any(stale)) {
    message(
      "extend(): ignoring anchors at or before the last history date (",
      last,
      "): ",
      stringr::str_flatten(format(dates[stale]), collapse = ", ")
    )
    dates <- dates[!stale]
    values <- values[!stale]
  }
  for (i in seq_along(dates)) {
    x[dates[i]] <- values[i]
  }
  if (hold_last && length(dates) > 0) {
    x[paste0(max(dates), "/")] <- values[which.max(dates)]
  }
  x <- if (interp == "linear") zoo::na.approx(x) else stine_fill(x)
  x %>%
    tsbox::ts_bind(make_xts(per = per)) %>%
    tsbox::ts_span(end = bank_end())
}


#' Fill missing values with the Stineman spline, by position
#'
#' Values after the last observation carry it forward; values before the
#' first stay missing.
#' @noRd
stine_fill <- function(x) {
  v <- as.numeric(x)
  have <- which(!is.na(v))
  if (length(have) < 2) {
    return(x)
  }
  fit <- stinepack::stinterp(have, v[have], seq_along(v))$y
  after <- seq_along(v) > max(have)
  fit[after] <- v[max(have)]
  miss <- is.na(v)
  v[miss] <- fit[miss]
  x[] <- v
  x
}


#' Compound a period growth path onto x from last
#' @noRd
compound_from <- function(x, growth, last, smooth, from) {
  if (!is.null(smooth)) {
    growth <- smooth_window(growth, smooth$span, from, smooth$to)
  }
  x[p("", last)] %>%
    tsbox::ts_chain(tsbox::ts_compound(growth[p(last, "")]))
}


#' Replace a window of a series with a loess fit on time
#'
#' Missing values in the window stay missing. The fit need not pass through
#' the values at the window's edges.
#' @noRd
smooth_window <- function(x, span, from = NULL, to = NULL) {
  window <- p(if (is.null(from)) "" else from, if (is.null(to)) "" else to)
  y <- x[window]
  t <- seq_along(y)
  v <- as.numeric(y)
  fit <- stats::loess(v ~ t, span = span)
  x[window] <- dplyr::if_else(
    is.na(v),
    NA_real_,
    stats::predict(fit, data.frame(t = t))
  )
  x
}
