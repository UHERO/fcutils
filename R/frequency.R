#' Interpolate a single vector containing missing values based on a pattern
#'
#' @keywords internal
#' @param vec_incomplete numeric vector with NAs to be filled
#' @param vec_pattern numeric vector representing the pattern
#' @return numeric vector with NAs filled
interpol_1 <- function(vec_incomplete, vec_pattern) {
  # return immediately if no work is needed
  if (!any(is.na(vec_incomplete))) {
    return(vec_incomplete)
  }

  # create a data frame to work with
  dat_tbl <- dplyr::tibble(
    idx = 1:length(vec_incomplete),
    vec_incomplete = vec_incomplete,
    vec_pattern = vec_pattern
  )

  # Step 1: find the boundaries of each gap using tidyr::fill()
  dat_tbl <- dat_tbl %>%
    dplyr::mutate(
      last_known_inc = dplyr::if_else(
        is.na(vec_incomplete),
        NA_real_,
        vec_incomplete
      ),
      next_known_inc = .data$last_known_inc
    ) %>%
    tidyr::fill(.data$last_known_inc, .direction = "down") %>%
    tidyr::fill(.data$next_known_inc, .direction = "up") %>%
    # use group_by and mutate to get the corresponding boundary values for vec_pattern and idx
    dplyr::group_by(grp = cumsum(!is.na(vec_incomplete))) %>%
    dplyr::mutate(
      last_known_pat = dplyr::first(vec_pattern),
      last_known_idx = dplyr::first(.data$idx)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"grp") %>%
    dplyr::group_by(grp = rev(cumsum(!is.na(rev(vec_incomplete))))) %>%
    dplyr::mutate(
      next_known_pat = dplyr::last(vec_pattern),
      next_known_idx = dplyr::last(.data$idx)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"grp")

  # Step 2: apply interpolation logic to all NAs at once
  dat_tbl <- dat_tbl %>%
    dplyr::mutate(
      interpolated_val = dplyr::case_when(
        # CASE 1: INTERNAL GAP (including correction for growth mismatch)
        !is.na(last_known_inc) &
          !is.na(next_known_inc) &
          last_known_idx != next_known_idx ~
          {
            # total_q_growth <- next_known_inc / last_known_inc
            # total_c_growth <- next_known_pat / last_known_pat
            # adj_factor <- total_q_growth / total_c_growth
            # k <- idx - last_known_idx
            # n <- next_known_idx - last_known_idx
            # feathered_adj <- (adj_factor - 1) * (k / n) + 1
            # c_growth_to_i <- vec_pattern / last_known_pat
            # last_known_inc * c_growth_to_i * feathered_adj

            last_known_inc *
              (vec_pattern / last_known_pat) * # c_growth_to_i
              (((next_known_inc / last_known_inc) / # total_q_growth
                (next_known_pat / last_known_pat) - # total_c_growth
                1) *
                ((.data$idx - last_known_idx) / # k
                  (next_known_idx - last_known_idx)) + # n
                1)
          },
        # CASE 2: TRAILING GAP (forward fill)
        !is.na(last_known_inc) & is.na(next_known_inc) ~
          last_known_inc * (vec_pattern / last_known_pat),
        # CASE 3: LEADING GAP (backward fill)
        is.na(last_known_inc) & !is.na(next_known_inc) ~
          next_known_inc * (vec_pattern / next_known_pat),
        TRUE ~ NA_real_
      )
    )

  # Step 3. return the original values combined with the new interpolated values
  final_vec <- dplyr::if_else(
    is.na(dat_tbl$vec_incomplete),
    dat_tbl$interpolated_val,
    dat_tbl$vec_incomplete
  )

  return(final_vec)
}


#' Interpolate univariate or multivariate time series with missing values
#'
#' @param incomplete a ts-boxable object with NAs
#' @param pattern a ts-boxable object no missing values, or NULL (default)
#'   to fill the gaps inside each series along a straight line
#' @param extend_range logical. If TRUE, uses a full_join to potentially extend
#'   the date range. If FALSE (default), uses a left_join to only fill NAs
#'   within the original date range.
#'
#' @return interpolated object of the same type as the input
#' @export
#'
#' @details the function aligns two sets of time series by date and then
#' interpolates the first set using the second set as a pattern. Without a
#' pattern, missing values between a series' first and last observation are
#' filled linearly (by position, so each period counts the same); missing
#' dates in a regular series are added first, and leading and trailing gaps
#' stay empty. extend_range has no effect then.
#' The time-span of the pattern has to match or be larger
#' than the time-span of the incomplete data.
#'
#' @examples
#' # straight-line fill of an internal gap
#' make_xts("2025M7", val = c(1, 2, NA, 4, 5)) |>
#'   interpol()
#' monthly_data_example |>
#'   dplyr::filter(time > "2002-01-01") |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(
#'     value = dplyr::if_else(time > "2021-01-01" & time < "2022-01-01",
#'     NA_real_, value)
#'   )|>
#'   interpol(monthly_data_example, extend_range = TRUE)
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::mutate(
#'     value = dplyr::if_else(
#'       time > "2021-01-01" & time < "2022-01-01",
#'       NA_real_,
#'       value
#'     )
#'   ) |>
#'   interpol(
#'     # missing values in the pattern
#'     monthly_data_example |>
#'       tsbox::ts_long() |>
#'       dplyr::mutate(
#'         value = dplyr::if_else(
#'           time > "2021-03-01" & time < "2021-09-01",
#'           NA_real_,
#'           value
#'         )
#'       )
#'   ) %>%
#'   # no interpolation where pattern has NAs
#'   tsbox::ts_wide()
interpol <- function(
  incomplete,
  pattern = NULL,
  extend_range = FALSE
) {
  if (is.null(pattern)) {
    return(interpol_linear(incomplete))
  }
  # convert to wide format and return additional details
  incomplete_mod <- conv_long(incomplete, ser_info = TRUE)
  incomplete_wide <- incomplete_mod %>%
    dplyr::mutate(id = stringr::str_c(.data$id, "_I")) %>%
    tsbox::ts_wide()
  pattern_wide <- conv_long(pattern) %>%
    dplyr::mutate(id = stringr::str_c(.data$id, "_P")) %>%
    tsbox::ts_wide()

  # make the join strategy an explicit parameter
  join_fun <- if (extend_range) dplyr::full_join else dplyr::left_join
  aligned_data <- join_fun(incomplete_wide, pattern_wide, by = "time") %>%
    dplyr::arrange(.data$time)

  # create a robust mapping of incomplete columns to pattern columns by name
  inc_cols <- names(incomplete_wide)[-1]
  pat_cols <- names(pattern_wide)[-1]

  # check that all needed pattern columns exist
  if (length(inc_cols) > 1 & length(pat_cols) == 1) {
    pat_cols <- rep(pat_cols, length(inc_cols))
    warning(
      "The same pattern is used for all interpolated series."
    )
  } else if (length(inc_cols) != length(pat_cols)) {
    stop(
      "Mismatch: could not find a pattern for every series."
    )
  }

  # mapping betwen incomplete series and pattern series
  col_map <- stats::setNames(
    pat_cols, # pattern column names
    inc_cols # incomplete column name
  )

  cat(
    "The mapping betwen interpolated series and pattern series is the following:\n"
  )
  print(col_map, quote = FALSE)

  # carry out the interpolation using the interpol_1() function
  interpolated_data <- aligned_data %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(inc_cols),
      .fns = ~ interpol_1(
        vec_incomplete = .x,
        # dynamically get the correct pattern column
        vec_pattern = get(col_map[dplyr::cur_column()])
      )
    )) %>%
    dplyr::select(c("time", dplyr::all_of(inc_cols))) %>%
    dplyr::rename_with(
      .cols = dplyr::all_of(inc_cols),
      .fn = ~ stringr::str_remove(.x, "_I$")
    )

  # return the final data, filtered to the original date range if not extending
  if (!extend_range) {
    interpolated_data <- interpolated_data %>%
      dplyr::semi_join(incomplete_wide, by = "time")
  }

  # reclass the output to match the input
  ans <- if (attr(incomplete_mod, "was_wide")) {
    interpolated_data
  } else {
    conv_long(interpolated_data) %>% tsbox::copy_class(incomplete)
  }

  return(ans)
}


#' Fill gaps inside series by linear interpolation (`interpol()` without a
#' pattern)
#'
#' @param x ts-boxable object
#'
#' @return object of the same type as x; missing values between the first
#'   and last observation of each series are filled along a straight line,
#'   leading and trailing ones are kept
#' @noRd
interpol_linear <- function(x) {
  x_mod <- conv_long(x, ser_info = TRUE)
  filled <- x_mod %>%
    tsbox::ts_regular() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(value = {
      pos <- seq_along(.data$value)
      have <- !is.na(.data$value)
      if (sum(have) < 2) {
        .data$value
      } else {
        stats::approx(pos[have], .data$value[have], xout = pos, rule = 1)$y
      }
    }) %>%
    dplyr::ungroup()
  reclass(filled, x_mod, x)
}


#' Disaggregate a single time series from low to high frequency
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


#' Disaggregate univariate or multivariate time series from low to high frequency
#'
#' @param x a tx-boxable object at a low frequency (e.g. annual or quarterly)
#' @param conv_type match the interpolated value via "first", "last", "sum",
#' "mean". If conv_type == "uhero" then the name of the time series x is
#' compared to the internal variable `sum_pattern`. For matching series names
#' the interpolation is based on "sum"; for all others it is based on "mean."
#' @param target_freq target frequency "quarter" or "month"
#' @param pattern a single high-frequency pattern that the interpolation should follow
#' @param extend_with NULL (default), "last_growth", or a ts-boxable object. Without
#'   a pattern the interpolation has nothing to follow at the end of a series
#'   and bends toward zero growth over the last periods (and the first).
#'   "last_growth" first continues each series at its last growth rate for three
#'   periods; a series (one per series in x, with the same names, or a single
#'   one for a single x) continues x with its growth at both ends, wherever
#'   it reaches beyond x. The result is cut back to x's own periods.
#'
#' @return interpolated object of the same type as the input
#' @export
#'
#' @details the time-span of the high-frequency pattern has to match or be larger
#' than the time-span of the low frequency series. NA values are not allowed,
#' except that with extend_with, leading and trailing NAs of x are dropped.
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
#' # the last year without and with an extension
#' a <- make_xts(2015, val = 100 * 1.02^(0:10))
#' disagg(a) |> tsbox::ts_pc() |> tail(4)
#' disagg(a, extend_with = "last_growth") |> tsbox::ts_pc() |> tail(4)
disagg <- function(
  x,
  conv_type = "mean",
  target_freq = "quarter",
  pattern = NULL,
  extend_with = NULL
) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # continue each series before interpolating; keep its span to cut back to
  if (!is.null(extend_with)) {
    info <- attributes(x_mod)[c("was_wide", "ser_names")]
    x_mod <- tidyr::drop_na(x_mod, "value")
    x_span <- tsbox::ts_summary(x_mod) %>%
      dplyr::mutate(
        last_day = purrr::map2_vec(
          .data$end,
          .data$diff,
          ~ lubridate::ceiling_date(.x, unit = .y) %>% lubridate::rollback()
        )
      ) %>%
      dplyr::select("id", "start", "last_day")
    x_mod <- extend_low(x_mod, extend_with)
    attr(x_mod, "was_wide") <- info$was_wide
    attr(x_mod, "ser_names") <- info$ser_names
  }
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

  # cut the extension back off
  if (!is.null(extend_with)) {
    if (!"id" %in% names(x_mod_int)) {
      x_mod_int <- dplyr::mutate(x_mod_int, id = x_span$id[1], .before = 1)
    }
    x_mod_int <- x_mod_int %>%
      dplyr::inner_join(x_span, by = "id") %>%
      dplyr::filter(.data$time >= .data$start, .data$time <= .data$last_day) %>%
      dplyr::select("id", "time", "value")
    if (nrow(x_span) == 1) {
      x_mod_int <- dplyr::select(x_mod_int, -"id")
    }
  }

  # reclass the output to match the input
  ans <- reclass(x_mod_int, x_mod, x)

  return(ans)
}


#' Continue low-frequency series before interpolation (used by `disagg()`)
#'
#' @param x_long long tibble (id, time, value) without missing values
#' @param extend_with "last_growth" or a ts-boxable object (see `disagg()`)
#' @param n_hold number of periods added by "last_growth"
#'
#' @return long tibble, each series continued after its last observation
#'   ("last_growth"), or at both ends as far as extend_with reaches (a series)
#' @noRd
extend_low <- function(x_long, extend_with, n_hold = 3) {
  ids <- unique(x_long$id)
  if (identical(extend_with, "last_growth")) {
    ext <- purrr::map(ids, function(i) {
      s <- dplyr::filter(x_long, .data$id == i) %>% dplyr::arrange(.data$time)
      n <- nrow(s)
      if (n < 2) {
        return(s)
      }
      step <- tsbox::ts_summary(s)$diff
      growth <- s$value[n] / s$value[n - 1]
      dates <- seq(s$time[n], by = step, length.out = n_hold + 1)[-1]
      dplyr::bind_rows(
        s,
        tibble::tibble(id = i, time = dates, value = s$value[n] * growth^(1:n_hold))
      )
    })
    return(dplyr::bind_rows(ext))
  }
  if (is.character(extend_with)) {
    stop("disagg(): extend_with must be NULL, \"last_growth\" or a series", call. = FALSE)
  }
  ew <- conv_long(extend_with) %>% tidyr::drop_na("value")
  if (!"id" %in% names(ew)) {
    ew <- dplyr::mutate(ew, id = ids[1], .before = 1)
  }
  if (length(ids) == 1 && length(unique(ew$id)) == 1) {
    ew$id <- ids
  }
  purrr::map(ids, function(i) {
    a <- dplyr::filter(x_long, .data$id == i)
    b <- dplyr::filter(ew, .data$id == i)
    if (nrow(b) == 0) {
      stop("disagg(): extend_with has no series ", i, call. = FALSE)
    }
    chain_1(a, b, method = "ratio", direction = "both")
  }) %>%
    dplyr::bind_rows()
}


#' Linear interpolation based on AREMOS command reference page 292
#' (superseded by disagg())
#'
#' @param ser_in the xts series to be interpolated (freq = a)
#' @param aggr interpolation method: aggregate via mean (default) or sum
#'
#' @return interpolated xts series (freq = q). A name ending in `.A`, `_A`,
#'   `.SOLA` or `_SOLA` gets the quarterly ending (`.Q`, `_Q`, `.SOLQ`,
#'   `_SOLQ`); other names are kept.
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
    stringr::str_replace("([._])SOLA$", "\\1SOLQ") %>%
    stringr::str_replace("([._])A$", "\\1Q")
  return(ser_out)
}


#' Aggregate from quarterly to annual frequency
#' (superseded by tsbox::ts_frequency())
#'
#' @param ser_in the xts series to be converted (freq = q)
#' @param	aggr aggregate via mean (default) or sum
#'
#' @return converted xts series (freq = a). A name ending in `.Q`, `_Q`,
#'   `.SOLQ` or `_SOLQ` gets the annual ending; other names are kept.
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
    stringr::str_replace("([._])SOLQ$", "\\1SOLA") %>%
    stringr::str_replace("([._])Q$", "\\1A")
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

  # one ts_frequency() call per aggregation type, not one per series
  x_mod_agg <- aggr_long(x_mod, conv_type, target_freq, na_rm)

  # the per-series version it replaces (one ts_frequency() call per series):
  # # convert to nested tibble and aggregate
  # x_mod_agg <- x_mod %>%
  #   tidyr::nest(data = c("id":"value"), .by = "id") %>%
  #   dplyr::mutate(
  #     # add a column with aggregation type
  #     agg_type = if (
  #       stringr::str_to_lower(conv_type) %>% stringr::str_detect("^u")
  #     ) {
  #       dplyr::if_else(
  #         .data$id %>% stringr::str_detect(sum_pattern),
  #         "sum",
  #         "mean"
  #       )
  #     } else {
  #       conv_type
  #     }
  #   ) %>%
  #   # operation on the nested column
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     aggd = list(tsbox::ts_frequency(
  #       .data$data,
  #       to = target_freq,
  #       aggregate = .data$agg_type,
  #       na.rm = na_rm
  #     ))
  #   ) %>%
  #   # only keep the aggregated data
  #   dplyr::select("aggd") %>%
  #   tidyr::unnest("aggd")

  # reclass the output to match the input
  ans <- reclass(x_mod_agg, x_mod, x)

  return(ans)
}


#' Aggregate univariate or multivariate fiscal time series from low to high frequency
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
#' @details date stamps typically reflect the start of a period, but here the
#' date stamps in the result are shifted 6 months forward relative to calendar
#' dates, so Jul21 becomes Jan22, or 2022-01-01, and 21Q3 becomes 22Q1.
#'
#' @examples
#' monthly_data_example |>
#'   faggr(conv_type = "sum", target_freq = "quarter")
#' monthly_data_example |>
#'   faggr(conv_type = "uhero", target_freq = "quarter")
#' # works with a single series too
#' monthly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("VISNS_HI") |>
#'   faggr(conv_type = "uhero", target_freq = "year") |>
#'   tsbox::ts_plot()
faggr <- function(
  x,
  conv_type = "mean",
  target_freq = "year",
  na_rm = FALSE
) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # shift by six months, so that each fiscal year (July to June) falls in
  # the calendar year it ends in, then aggregate as aggr() does
  x_mod_agg <- x_mod %>%
    tsbox::ts_lag("6 months") %>%
    aggr_long(conv_type, target_freq, na_rm)

  # the per-series, per-year version it replaces (one ts_frequency() call per
  # series and year; it stopped on a year with no values):
  # # convert to nested tibble and aggregate
  # x_mod_agg <- x_mod %>%
  #   tsbox::ts_lag("6 months") %>%
  #   dplyr::mutate(yr = lubridate::floor_date(.data$time, "year")) %>%
  #   tidyr::nest(data = c("id":"value"), .by = c("id", "yr")) %>%
  #   dplyr::mutate(
  #     # add a column with aggregation type
  #     agg_type = if (
  #       stringr::str_to_lower(conv_type) %>% stringr::str_detect("^u")
  #     ) {
  #       dplyr::if_else(
  #         .data$id %>% stringr::str_detect(sum_pattern),
  #         "sum",
  #         "mean"
  #       )
  #     } else {
  #       conv_type
  #     }
  #   ) %>%
  #   # operation on the nested column
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     aggd = list(tsbox::ts_frequency(
  #       .data$data,
  #       to = target_freq,
  #       aggregate = .data$agg_type,
  #       na.rm = na_rm
  #     ))
  #   ) %>%
  #   # only keep the aggregated data
  #   dplyr::select("aggd") %>%
  #   tidyr::unnest("aggd") #%>%
  # # tsbox::ts_lag("-6 months")

  # reclass the output to match the input
  ans <- reclass(x_mod_agg, x_mod, x)

  return(ans)
}


#' Aggregate long data, one ts_frequency() call per aggregation type
#'
#' @param x_mod long tibble (id, time, value)
#' @param conv_type "mean", "sum", ... or "uhero" (sum for names matching
#'   `sum_pattern`, mean otherwise)
#' @return long tibble, series in their original order
#' @noRd
aggr_long <- function(x_mod, conv_type, target_freq, na_rm) {
  ids <- unique(x_mod$id)
  agg_type <- if (stringr::str_detect(stringr::str_to_lower(conv_type), "^u")) {
    dplyr::if_else(stringr::str_detect(ids, sum_pattern), "sum", "mean")
  } else {
    rep(conv_type, length(ids))
  }
  purrr::map(unique(agg_type), function(type) {
    x_mod %>%
      dplyr::filter(.data$id %in% ids[agg_type == type]) %>%
      tsbox::ts_frequency(to = target_freq, aggregate = type, na.rm = na_rm)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(match(.data$id, ids), .data$time)
}
