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
#' @return TRUE for a data frame with a `time` column and numeric value
#'   columns only, FALSE for a long data frame or any object that is not a
#'   data frame. A data frame without a `time` column, or without a numeric
#'   column, stops with an error.
#' @export
#'
#' @examples
#' monthly_data_example |> is_wide()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   is_wide()
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_tslist() |>
#'   is_wide()
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


#' TRUE if no series in a long table misses a date
#'
#' Each series must be one sorted run of Dates, equally spaced in days (daily,
#' weekly) or in months on the same day of the month (monthly, quarterly,
#' annual). Then `tsbox::ts_regular()` would return the rows unchanged.
#'
#' @param x long tibble with columns (id,) time, value
#' @noRd
dates_complete <- function(x) {
  if (!(identical(names(x), c("id", "time", "value")) ||
    identical(names(x), c("time", "value")))) {
    return(FALSE)
  }
  t <- x$time
  if (!inherits(t, "Date") || anyNA(t)) {
    return(FALSE)
  }
  id <- if ("id" %in% names(x)) as.character(x$id) else rep("", length(t))
  runs <- rle(id)
  # each series must be one contiguous block
  if (anyDuplicated(runs$values)) {
    return(FALSE)
  }
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  days <- as.numeric(t)
  months <- lubridate::year(t) * 12L + lubridate::month(t)
  mday <- lubridate::mday(t)
  all(purrr::map2_lgl(starts, ends, function(s, e) {
    if (e - s < 1L) {
      return(TRUE)
    }
    i <- s:e
    dd <- diff(days[i])
    if (all(dd > 0) && max(dd) - min(dd) < 1) {
      return(TRUE)
    }
    dm <- diff(months[i])
    all(dm > 0) && all(dm == dm[1]) && all(mday[i] == mday[s])
  }))
}


#' Return a long result in the class of the input
#'
#' The other half of `conv_long(x, ser_info = TRUE)`: wide if x was wide,
#' otherwise x's own class.
#'
#' @param out long tibble (id, time, value)
#' @param x_mod x as returned by `conv_long(x, ser_info = TRUE)`
#' @param x the original input
#' @noRd
reclass <- function(out, x_mod, x) {
  if (attr(x_mod, "was_wide")) {
    tsbox::ts_wide(out)
  } else {
    tsbox::copy_class(out, x)
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
#' `ser_names` are the names of the series in x. Missing dates inside each
#' series are filled with NA rows. A single unnamed series gets the id
#' "x_mod".
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
#' long <- quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   conv_long(ser_info = TRUE)
#' attr(long, "was_wide")
#' attr(long, "ser_names") |> head()
#' # a single series still gets an id column
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   tsbox::ts_xts() |>
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
  # (read from the id column when there is one: ts_summary() computes much
  # more than the names and is slow on large banks)
  # (only needed when x does not carry a single name itself)
  ser_names <- if (length(ser_names_1) == 1) {
    ser_names_1
  } else if (identical(names(x_mod)[1], "id")) {
    unique(as.character(x_mod$id))
  } else {
    tsbox::ts_summary(x_mod) %>%
      dplyr::pull(.data$id)
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
    tsbox::ts_default()
  # ts_regular() fills missing dates; it is slow on large banks even when no
  # date is missing, because it converts monthly and quarterly data back and
  # forth to check them
  x_mod <- if (dates_complete(x_mod)) {
    # what ts_regular() returns for complete dates, without its checks
    tsbox::copy_class(tsbox::ts_dts(x_mod), x_mod)
  } else {
    tsbox::ts_regular(x_mod)
  }

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
#'   conv_wide() |>
#'   dplyr::select(1:4)
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
#'   conv_xts() |>
#'   tsbox::ts_pick(1:3) |>
#'   tail(3)
#' quarterly_data_example |>
#'   conv_wide() |>
#'   conv_xts() |>
#'   tsbox::ts_pick(1:3) |>
#'   tail(3)
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
#'   conv_tslist() |>
#'   names() |>
#'   head()
#' quarterly_data_example |>
#'   conv_wide() |>
#'   conv_tslist() |>
#'   utils::str(list.len = 2)
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
