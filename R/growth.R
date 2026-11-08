#' Convert annualized growth to quarterly growth
#'
#' @param x ts-boxable object containing annualized growth (in percent)
#' @param freq numeric frequency of the time series e.g. 4 for quarterly
#'
#' @return object of the same type as the input containing quarterly growth (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pca() |>
#'   pca_to_pc() |>
#'   tail()
#' tsbox::ts_c(
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pca() |>
#'     pca_to_pc(),
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pc()
#' ) |>
#'   dplyr::arrange(id, time) |>
#'   tsbox::ts_wide()
pca_to_pc <- function(x, freq = 4) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_pc <- x_mod %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^(1 / freq) - 1) * 100)

  # reclass the output to match the input
  ans <- reclass(x_mod_pc, x_mod, x)

  return(ans)
}


#' Convert quarterly growth to annualized growth
#'
#' @param x ts-boxable object containing quarterly growth (in percent)
#' @param freq numeric frequency of the time series e.g. 4 for quarterly
#'
#' @return object of the same type as the input containing annualized growth (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pc() |>
#'   pc_to_pca() |>
#'   tail()
#' tsbox::ts_c(
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pc() |>
#'     pc_to_pca(),
#'   quarterly_data_example |>
#'     tsbox::ts_long() |>
#'     tsbox::ts_pca()
#' ) |>
#'   dplyr::arrange(id, time) |>
#'   tsbox::ts_wide()
pc_to_pca <- function(x, freq = 4) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_pc <- x_mod %>%
    dplyr::mutate(value = ((1 + .data$value / 100)^freq - 1) * 100)

  # reclass the output to match the input
  ans <- reclass(x_mod_pc, x_mod, x)

  return(ans)
}


#' Calculate multi-period average growth
#'
#' @param x ts-boxable object for which growth is calculated (in levels)
#' @param lag number of periods over which growth is calculated
#' @param comp_freq compounding frequency (1 if period by period, 4 if annualized for quarterly data, etc.)
#'
#' @return object of the same type as the input ontaining the average growth of x (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   pcmp(20) |>
#'   tail()
#' quarterly_data_example |>
#'   pcmp(4, 4) |>
#'   tail()
#' quarterly_data_example |>
#'   pcmp(1, 4) |>
#'   tail()
pcmp <- function(x, lag = 4, comp_freq = 1) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # x_mod_pcmp <- x_mod %>%
  #   tsbox::ts_tslist() %>%
  #   purrr::map(
  #     ~ (((.x / tsbox::ts_lag(.x, lag))^(comp_freq / lag)) - 1) * 100
  #   ) %>%
  #   set_attr_tslist() %>%
  #   # univariate data requires special treatment
  #   {
  #     if (length(attr(x_mod, "ser_names")) == 1) {
  #       tsbox::ts_tbl(.) %>%
  #         tsbox::ts_long() %>%
  #         dplyr::mutate(id = attr(x_mod, "ser_names"))
  #     } else {
  #       tsbox::ts_tbl(.)
  #     }
  #   }

  # x_mod %ts/% tsbox::ts_lag(x_mod, lag), by position when no date is missing
  x_mod_pcmp <- lag_ratio(x_mod, lag) %>%
    dplyr::mutate(value = ((.data$value^(comp_freq / lag)) - 1) * 100)

  # the per-series version it replaces:
  # # convert to nested tibble and calculate
  # x_mod_pcmp <- x_mod %>%
  #   tidyr::nest(data = c("id":"value"), .by = "id") %>%
  #   # operation on the nested column
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     calc = list((((.data$data %ts/% tsbox::ts_lag(.data$data, lag)))))
  #     #^(comp_freq / lag)) - 1) * 100)
  #   ) %>%
  #   # only keep the calculated data
  #   dplyr::select("calc") %>%
  #   tidyr::unnest("calc") %>%
  #   dplyr::mutate(value = ((.data$value^(comp_freq / lag)) - 1) * 100)

  # reclass the output to match the input
  ans <- reclass(x_mod_pcmp, x_mod, x)

  return(ans)
}


#' Calculate compund annual growth
#'
#' @param x ts-boxable object for which growth is calculated between first and last period
#'
#' @return a tibble with a single row containing the compound annual growth between
#'   the first and last period of x (in percent)
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   cagr()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   cagr()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_xts() |>
#'   tsbox::ts_span("2000-01-01", "2020-01-01") |>
#'   tsbox::ts_pick("E_NF_HI") |>
#'   cagr()
cagr <- function(x) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  # convert to wide format, keep first and last row only
  # calculate time difference in years and ratio of values
  # apply the cagr formula to all data columns
  x_mod_cagr <- x_mod %>%
    tsbox::ts_wide() %>%
    dplyr::slice(1, dplyr::n()) %>%
    dplyr::summarize(
      dplyr::across(
        "time",
        ~ difftime(get(dplyr::cur_column())[2], get(dplyr::cur_column())[1]) %>%
          lubridate::time_length(unit = "years")
      ),
      dplyr::across(
        !"time",
        ~ get(dplyr::cur_column())[2] / get(dplyr::cur_column())[1]
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      !"time",
      ~ (get(dplyr::cur_column())^(1 / .data$time) - 1) * 100
    )) %>%
    dplyr::rename("years_elapsed" = "time")

  return(x_mod_cagr)
}


#' Extend a series using year over year growth
#'
#' @param yoy_gr ts-boxable object containing year over year growth rates
#' @param hist_lev ts-boxable object containing the history in levels
#' for forecast and at least one year of history (in percent)
#' @param smooth_span extent of smoothing between 0-1 (default: 0, no smoothing).
#' Effect depends on the length of the time series. Low smooth_span value may
#' trigger warning, if time series too short.
#'
#' @return object of the same type as hist_lev extended with year over year growth
#' @export
#'
#' @details This function only works for univariate time series and requires
#' that the growth rates are available for at least the last year of history.
#' Year-over-year growth rates propagate the fluctuations of the base period
#' into the extension period. This can be mitigated by smoothing the extension.
#' The inputs should not contain missing values.
#'
#' @examples
#' gr <- quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::filter(id == "E_NF_HI") |>
#'   tsbox::ts_pcy()
#' lev <- quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   dplyr::filter(id == "ECT_HI")
#' res1 <- yoy_to_lev(gr, lev |> dplyr::filter(time <= "2010-01-01"))
#' res2 <- yoy_to_lev(gr, lev |> dplyr::filter(time <= "2010-01-01"), 1/8)
#' tsbox::ts_plot(lev, res1, res2)
yoy_to_lev <- function(yoy_gr, hist_lev, smooth_span = 0) {
  # convert to long format and return additional details
  yoy_gr_mod <- conv_long(yoy_gr, ser_info = TRUE)
  hist_lev_mod <- conv_long(hist_lev, ser_info = TRUE)

  # find the start and end of the base period
  base_per_end <- find_end(hist_lev_mod)
  base_per_start <- base_per_end - lubridate::years(1)

  # make sure the growth series starts at or before the start of the base period
  if (find_start(yoy_gr_mod) > base_per_start) {
    stop(
      "The start of the growth series is after the start of the base period.
         Extend the growth series backward with 0% growth in the base period."
    )
  }

  # extract the history in the base period
  hist_lev_base <- hist_lev_mod %>%
    dplyr::filter(.data$time > base_per_start, .data$time <= base_per_end) %>%
    dplyr::mutate(month_id = lubridate::month(.data$time))

  # calculate the cumulative growth and levels
  calcs <- yoy_gr_mod %>%
    dplyr::filter(.data$time > base_per_start) %>%
    dplyr::mutate(month_id = lubridate::month(.data$time)) %>%
    # cumulative growth for each period (works for monthly and quarterly data)
    dplyr::mutate(
      cum_gr = cumprod(1 + .data$value / 100),
      .by = "month_id"
    ) %>%
    # store the cumulative growth in the base period
    dplyr::left_join(
      dplyr::filter(
        .,
        .data$time > base_per_start,
        .data$time <= base_per_end
      ) %>%
        dplyr::select("month_id", "base_gr" = "cum_gr"),
      by = "month_id"
    ) %>%
    # add the history in the base period
    dplyr::left_join(
      hist_lev_base %>%
        dplyr::select("month_id", "base_val" = "value"),
      by = "month_id"
    ) %>%
    dplyr::mutate(
      # adjust the cumulative growth so it is 1 in the base period
      cum_gr_adj = .data$cum_gr / .data$base_gr,
      # calculate levels using the adjusted cumulative growth applied to the base period
      lev_val = .data$base_val * .data$cum_gr_adj
    ) %>%
    # select the relevant columns
    dplyr::select("id", "time", "value" = "lev_val") %>%
    # name the growth series the same as the history
    dplyr::mutate(id = attr(hist_lev_mod, "ser_names"))

  # extend history with period to period growth of the calculated levels
  # and apply smoothing to extension if requested
  if (smooth_span == 0) {
    ext_lev_mod <- hist_lev_mod %>%
      tsbox::ts_chain(calcs)
  } else {
    ext_lev_mod <- hist_lev_mod %>%
      tsbox::ts_chain(calcs) %>%
      dplyr::mutate(
        value_smooth = stats::loess(
          value ~ time %>% as.numeric(),
          data = .,
          span = smooth_span
        ) %>%
          stats::predict(),
        value = dplyr::if_else(
          .data$time > base_per_end,
          .data$value_smooth,
          .data$value
        )
      ) %>%
      dplyr::select(-"value_smooth")
  }

  # reclass the output to match the input
  ans <- reclass(ext_lev_mod, hist_lev_mod, hist_lev)

  return(ans)
}


#' Index series to a base period (wrapper around tsbox::ts_index())
#'
#' Sets each series to `base_value` in the base period and scales the rest of
#' the series by the same factor. Formerly `index()`, renamed because it masked
#' `zoo::index()` and `xts::index()`; then `ts_rebase()` in fcutils 0.1.0 (the
#' `ts_` prefix is tsbox's) and `rebase()` in 0.1.1 and 0.1.2 (rebasing
#' re-expresses a real series in the prices of another year, which this
#' function does not do).
#'
#' @param x ts-boxable object to be indexed
#' @param base_per base date when the index is set to base_value (see examples).
#'   If two dates are provided, the mean in the range is set equal to base_value.
#' @param base_value numeric value of the index at base_per (e.g. 1 or 100)
#'
#' @return indexed object of the same type as the input
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   index_to_base(2010.1)
#' quarterly_data_example |>
#'   index_to_base(c(2010.1, 2010.4))
#' quarterly_data_example |>
#'   index_to_base(c("2010-01-01", "2010-12-31"), 1)
index_to_base <- function(x, base_per = as.character(Sys.Date()), base_value = 100) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_index <- x_mod %>%
    tsbox::ts_index(base = to_ymd(base_per)) %>%
    dplyr::mutate(value = .data$value * base_value) %>%

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
  ans <- reclass(x_mod_index, x_mod, x)

  return(ans)
}


#' Backward looking moving average
#'
#' @param x ts-boxable object
#' @param order numeric order (window length) of moving average, includes contemporaneous observation
#'
#' @return object of the same type as the input containing moving average
#' @export
#'
#' @examples
#' quarterly_data_example |>
#'   ma(4) |>
#'   head()
ma <- function(x, order) {
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  x_mod_ma <- x_mod %>%
    dplyr::mutate(
      value = slider::slide_dbl(
        .data$value,
        mean,
        .before = order - 1,
        .complete = TRUE
      ),
      .by = "id"
    )

  # reclass the output to match the input
  ans <- reclass(x_mod_ma, x_mod, x)

  return(ans)
}
