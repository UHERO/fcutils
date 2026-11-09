# **************************
# time series utility functions ----
# **************************

#' Rename y's series after their partners in x (used by `chain()`)
#'
#' @param x_wide,y_wide wide tibbles with a time column
#' @param how "name" or "stem"
#'
#' @return y_wide, with partner series renamed to x's names
#' @noRd
match_names <- function(x_wide, y_wide, how) {
  x_names <- setdiff(colnames(x_wide), "time")
  y_names <- setdiff(colnames(y_wide), "time")
  # one series each: they are partners whatever their names
  if (length(x_names) == 1 && length(y_names) == 1) {
    return(dplyr::rename(y_wide, !!x_names := dplyr::all_of(y_names)))
  }
  if (how == "name") {
    return(y_wide)
  }
  stem <- function(nm) stringr::str_remove(nm, "[._](SOL)?[AQM]$")
  for (side in list(list(x_names, "x"), list(y_names, "y"))) {
    dup <- unique(stem(side[[1]])[duplicated(stem(side[[1]]))])
    if (length(dup) > 0) {
      stop(
        "chain(): several series in ", side[[2]], " share the stem ",
        stringr::str_flatten(dup, collapse = ", "),
        call. = FALSE
      )
    }
  }
  partner <- x_names[match(stem(y_names), stem(x_names))]
  has <- !is.na(partner)
  # keep y's unmatched series under their own names
  new_names <- y_names
  new_names[has] <- partner[has]
  colnames(y_wide) <- c("time", new_names)[match(colnames(y_wide), c("time", y_names))]
  y_wide
}


#' Chain one series with another (used by `chain()`)
#'
#' @param a,b tibbles with columns id, time, value and no missing values
#' @param method "ratio" or "diff"
#' @param direction "both", "forward" or "backward"
#'
#' @return tibble with columns id, time, value: a, extended with b
#' @noRd
chain_1 <- function(a, b, method, direction) {
  if (method == "ratio" && direction == "both") {
    return(tsbox::ts_chain(a, b))
  }
  link <- function(t) {
    vb <- b$value[b$time == t]
    if (length(vb) != 1) {
      stop("chain(): y has no value at ", t, call. = FALSE)
    }
    va <- a$value[a$time == t]
    function(v) if (method == "ratio") v * va / vb else v + va - vb
  }
  out <- a
  if (direction %in% c("both", "forward")) {
    t1 <- max(a$time)
    tail <- b[b$time > t1, ]
    tail$value <- link(t1)(tail$value)
    out <- dplyr::bind_rows(out, dplyr::mutate(tail, id = a$id[1]))
  }
  if (direction %in% c("both", "backward")) {
    t0 <- min(a$time)
    head <- b[b$time < t0, ]
    head$value <- link(t0)(head$value)
    out <- dplyr::bind_rows(dplyr::mutate(head, id = a$id[1]), out)
  }
  dplyr::arrange(out, .data$time)
}

#' Method for each series (used by `chain()`)
#'
#' @param method one of "ratio", "diff" (or the default vector), or a named
#'   vector of them by series name
#' @return a function of a series name returning its method
#' @noRd
chain_methods <- function(method) {
  if (is.null(names(method))) {
    method <- match.arg(method, c("ratio", "diff"))
    return(function(id) method)
  }
  bad <- setdiff(method, c("ratio", "diff"))
  if (length(bad) > 0) {
    stop('chain(): method must be "ratio" or "diff"', call. = FALSE)
  }
  function(id) if (id %in% names(method)) method[[id]] else "ratio"
}

#' Extend "ts-boxable" objects by chaining (extension of `tsbox::ts_chain()`)
#'
#' @param x a "ts-boxable" object to be extended
#' @param y a "ts-boxable" object containing data to extend x
#' @param ids a character vector with series names to be extended (default: NULL,
#' in which case all series common to x and y are extended). Names not in
#' both x and y are skipped.
#' @param method "ratio" (default) scales y so that it meets x, which keeps
#' y's growth rates (for levels); "diff" shifts y by a constant, which keeps
#' y's changes (for rates such as the unemployment rate). Either one method
#' for all series, or a named vector giving the method by series name in x,
#' `c(UR_AT_US = "diff", FFR_AT_US = "diff")`; series not named use "ratio".
#' @param direction "both" (default) extends x at both ends, "forward" only
#' after its last observation, "backward" only before its first
#' @param match how series in x find their partner in y: "name" (default)
#' needs the same name; "stem" compares names without a final frequency or
#' solution ending (`_A`, `_Q`, `_M`, `_SOLA`, `_SOLQ`, `_SOLM`, or the same
#' with `.`), so `UR_AT_US_Q` pairs with `UR_AT_US_SOLQ`. A single series in
#' x always pairs with a single series in y. The result keeps x's names. x
#' and y must have the same frequency; chain() does not convert or check it.
#'
#' @param verbose if TRUE (default), list the series being extended
#' @return object of the same type as x with extended series. Series in y
#'   with no partner in x are dropped.
#' @export
#'
#' @details chain() joins data: two sources of the same series, or two banks.
#' To forecast one series from assumptions or a path, use [extend()]; see
#' "Which function" there.
#'
#' This function performs a similar operation to `tsbox::ts_chain()`,
#' but for multiple series. x and y are linked at x's first observation (going
#' backward) and its last (going forward). With `method = "diff"` or a single
#' direction, y needs a value at those dates, or chain() stops. The default,
#' method "ratio" with direction "both", is `tsbox::ts_chain()`: if y lacks a
#' value at those dates, it links at the nearest date where both have one.
#'
#' @examples
#' # extend two of the series; the others stay as they were
#' quarterly_data_example |>
#'   dplyr::filter(time < as.Date("2000-01-01")) |>
#'   chain(quarterly_data_example, ids = c("E_NF_HI", "ECT_HI")) |>
#'   dplyr::select(time, E_NF_HI, ECT_HI, EMN_HI) |>
#'   tail(3)
#' # extend all of them
#' quarterly_data_example |>
#'   dplyr::filter(time < as.Date("2000-01-01")) |>
#'   chain(quarterly_data_example, verbose = FALSE) |>
#'   dplyr::select(time, E_NF_HI, ECT_HI, EMN_HI) |>
#'   tail(3)
#' # continue an annual history with the changes of a forecast
#' hist <- make_xts(2020, val = c(8.1, 5.4, 3.6, 3.6))
#' fcst <- make_xts(2022, val = c(3.5, 3.7, 4.0, 4.2, 4.3))
#' chain(hist, fcst, method = "diff", direction = "forward")
#' # pair series by name stem: UR_A with UR_SOLA, LF_A with LF_SOLA
#' hist <- make_xts(2020, val = tibble::tibble(UR_A = c(8.1, 5.4, 3.6), LF_A = c(160, 161, 164)))
#' fcst <- make_xts(2022, val = tibble::tibble(UR_SOLA = c(4.0, 4.4, 4.9), LF_SOLA = c(165, 166, 168)))
#' chain(hist, fcst, match = "stem", direction = "forward")
#' # a method per series: changes for the rate (3.6 + 0.4, + 0.5), growth for
#' # the level
#' chain(hist, fcst, match = "stem", direction = "forward", method = c(UR_A = "diff"))
chain <- function(
  x,
  y,
  ids = NULL,
  method = c("ratio", "diff"),
  direction = c("both", "forward", "backward"),
  match = c("name", "stem"),
  verbose = TRUE
) {
  method_of <- chain_methods(method)
  direction <- match.arg(direction)
  match <- match.arg(match)
  # doubles throughout: chained values replace x's in place, which fails
  # for an integer column
  x_wide <- conv_wide(x) %>% dplyr::mutate(dplyr::across(-"time", as.double))
  unknown <- setdiff(names(method), colnames(x_wide))
  if (length(unknown) > 0) {
    stop(
      "chain(): method names series not in x: ",
      stringr::str_flatten(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  y_wide <- conv_wide(y) %>% dplyr::mutate(dplyr::across(-"time", as.double))
  y_wide <- match_names(x_wide, y_wide, match)

  # identify series to be extended
  if (is.null(ids)) {
    ids <- intersect(colnames(x_wide), colnames(y_wide)) %>%
      setdiff("time")
    if (length(ids) == 0L) {
      return(x)
    } else {
      if (verbose) {
        message(
          "Extending the following series: \n",
          stringr::str_flatten(ids, collapse = ", ")
        )
      }
    }
  } else {
    ids <- intersect(ids, colnames(x_wide)) %>%
      intersect(colnames(y_wide))
    if (length(ids) == 0L) {
      return(x)
    } else {
      if (verbose) {
        message(
          "Extending the following series: \n",
          stringr::str_flatten(ids, collapse = ", ")
        )
      }
    }
  }

  # extend x with y: chain_1() on each pair of columns, results placed back
  # by date (direct column access; a left_join() per series was slow)
  chained_x <- x_wide %>%
    dplyr::select(dplyr::all_of(c("time", ids))) %>%
    dplyr::full_join(
      y_wide %>%
        dplyr::select(dplyr::all_of(c("time", ids))),
      by = "time"
    ) %>%
    dplyr::arrange(.data$time)
  for (x_col in stringr::str_subset(names(chained_x), "\\.x$")) {
    y_col <- stringr::str_replace(x_col, "\\.x$", ".y")
    chained <- chain_1(
      tibble::tibble(
        id = x_col,
        time = chained_x$time,
        value = chained_x[[x_col]]
      ) %>%
        tidyr::drop_na(),
      tibble::tibble(
        id = y_col,
        time = chained_x$time,
        value = chained_x[[y_col]]
      ) %>%
        tidyr::drop_na(),
      method = method_of(stringr::str_remove(x_col, "\\.x$")),
      direction = direction
    )
    chained_x[[x_col]] <- chained$value[match(chained_x$time, chained$time)]
  }
  chained_x <- chained_x %>%
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "\\.x$"),
      .cols = dplyr::ends_with(".x")
    ) %>%
    dplyr::select(-dplyr::ends_with(".y"))

  # insert the chained columns into the original x_wide
  chained_x <- x_wide %>%
    dplyr::rows_upsert(
      chained_x,
      by = "time"
    )

  # reclass the output to match the input
  ans <- if (is_wide(x)) {
    chained_x
  } else {
    conv_long(chained_x) %>% tsbox::copy_class(x)
  }

  return(ans)
}
