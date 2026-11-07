# Reference implementations: the helpers in forecastr's R/extsol_funs.R and
# R/extsol_data.R as of November 2026, copied with namespaces added. The
# tests check that the new fcutils arguments reproduce them. They can go once
# extsol has moved to fcutils (step 6 of the plan).

ref_bnk_start <- as.Date("1970-01-01")
ref_bnk_end <- as.Date("2070-12-31")

# extsol's bank end, for the duration of one test
ref_bank <- function(env = parent.frame()) {
  withr::local_options(list(fcutils.bnk_end = ref_bnk_end), .local_envir = env)
}

# extsol series saved from forecastr's history files and its latest pins
extsol_fx <- function() readRDS(test_path("fixtures", "extsol.rds"))

ref_to_annual <- function(x) {
  tsbox::ts_frequency(x, "year") |>
    tsbox::ts_bind(make_xts(per = "year")) |>
    tsbox::ts_span(end = ref_bnk_end)
}

ref_annual_solution <- function(q, hist = NULL, rate = FALSE) {
  from_q <- ref_to_annual(q)
  if (is.null(hist) || all(is.na(hist))) return(from_q)
  hist <- tsbox::ts_na_omit(hist)
  last <- stats::end(hist)
  from_q <- tsbox::ts_na_omit(from_q)
  from_q <- tsbox::ts_span(from_q, start = last)
  out <- if (rate) {
    shift <- as.numeric(hist[last]) - as.numeric(from_q[last])
    tsbox::ts_bind(hist, from_q + shift)
  } else {
    tsbox::ts_chain(hist, from_q)
  }
  out |>
    tsbox::ts_bind(make_xts(per = "year")) |>
    tsbox::ts_span(ref_bnk_start, ref_bnk_end)
}

ref_splice_back <- function(new, old) {
  new <- dplyr::filter(new, !is.na(value))
  t0 <- min(new$time)
  ratio <- new$value[new$time == t0] / old$value[old$time == t0]
  old |>
    dplyr::filter(time < t0, !is.na(value)) |>
    dplyr::mutate(value = value * ratio) |>
    dplyr::bind_rows(new) |>
    dplyr::arrange(time)
}

ref_hold_last_growth <- function(x, n_years = 3) {
  x <- tsbox::ts_na_omit(x)
  level <- as.numeric(xts::last(x))
  growth <- level / as.numeric(x[nrow(x) - 1])
  ext <- tsbox::ts_xts(stats::ts(
    level * growth^(1:n_years),
    start = lubridate::year(find_end(x)) + 1
  ))
  tsbox::ts_bind(x, ext)
}

ref_to_quarterly <- function(x, extend_with = ref_hold_last_growth(x)) {
  x <- tsbox::ts_na_omit(x)
  first_q <- find_start(x)
  last_q <- find_end(x) + months(9)
  if (!is.null(extend_with)) {
    x <- tsbox::ts_chain(x, tsbox::ts_na_omit(extend_with))
  }
  disagg(x) |>
    tsbox::ts_span(first_q, last_q)
}

ref_fill_gaps <- function(x) {
  x <- dplyr::filter(x, !is.na(value))
  step <- if (stats::median(as.numeric(diff(x$time))) < 40) "month" else "quarter"
  tibble::tibble(time = seq(min(x$time), max(x$time), by = step)) |>
    dplyr::left_join(x, by = "time") |>
    dplyr::mutate(value = zoo::na.approx(value))
}

ref_addf_ramps <- function(ramps) {
  purrr::pmap(ramps, function(from, to, start, end) {
    addf(
      format(lubridate::yq(from)),
      format(lubridate::yq(to)),
      start,
      end,
      per = "quarter"
    )
  }) |>
    purrr::reduce(`+`, .init = addf(per = "quarter"))
}

# compare two single series on their common non-missing dates
same_values <- function(a, b, tol = 1e-10) {
  ab <- tsbox::ts_c(a = tsbox::ts_na_omit(a), b = tsbox::ts_na_omit(b)) |>
    tsbox::ts_na_omit()
  expect_gt(nrow(ab), 0)
  expect_equal(as.numeric(ab$a), as.numeric(ab$b), tolerance = tol)
  # and the same dates
  expect_equal(
    zoo::index(tsbox::ts_na_omit(a)),
    zoo::index(tsbox::ts_na_omit(b))
  )
}
