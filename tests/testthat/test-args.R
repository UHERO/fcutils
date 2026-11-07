# Step 3 of the plan: new arguments reproduce the extsol helpers.

test_that("multi_chain(method = 'diff', direction = 'forward') is annual_solution(rate = TRUE)", {
  ref_bank()
  fx <- extsol_fx()
  ref <- ref_annual_solution(fx$ur_q, fx$ur_a_hist, rate = TRUE)
  new <- quiet(multi_chain(
    fx$ur_a_hist, aggr(fx$ur_q),
    method = "diff", direction = "forward"
  ))
  same_values(new, ref)
  # and what extsol published
  same_values(new, fx$ur_a_sol)
})

test_that("multi_chain(direction = 'forward') is annual_solution() for levels", {
  ref_bank()
  fx <- extsol_fx()
  ref <- ref_annual_solution(fx$lf_q, fx$lf_a_hist)
  new <- quiet(multi_chain(fx$lf_a_hist, aggr(fx$lf_q), direction = "forward"))
  same_values(new, ref)
  # no check against the published LF: extsol chains it to the history after
  # the CPS population-break adjustment, which the fixture does not hold
})

test_that("multi_chain(direction = 'backward') is splice_back()", {
  ref_bank()
  fx <- extsol_fx()
  cpi <- tsbox::ts_tbl(fx$cpi_jp_m)
  new_part <- dplyr::filter(cpi, time >= as.Date("2010-01-01"))
  old_part <- dplyr::mutate(cpi, value = value * 0.9 + 0.01 * seq_along(value))
  ref <- ref_splice_back(new_part, old_part)
  new <- quiet(multi_chain(
    tsbox::ts_xts(new_part), tsbox::ts_xts(old_part),
    direction = "backward"
  ))
  same_values(new, tsbox::ts_xts(ref))
})

test_that("multi_chain() stops when y has no value at the link", {
  ref_bank()
  x <- make_xts(2020, val = c(1, 2, 3))
  y <- make_xts(2024, val = c(4, 5))
  expect_error(quiet(multi_chain(x, y, direction = "forward")), "no value at 2022-01-01")
})

test_that("multi_chain() defaults are unchanged (ts_chain)", {
  ref_bank()
  x <- make_xts(2020, val = c(10, 11, 12))
  y <- make_xts(2018, val = c(1, 1.1, 1.2, 1.3, 1.4, 1.5))
  same_values(quiet(multi_chain(x, y)), tsbox::ts_chain(x, y))
})

test_that("disagg(extend_with = 'hold') is to_quarterly()", {
  ref_bank()
  fx <- extsol_fx()
  x <- fx$gdppc_jp_a
  same_values(disagg(x, extend_with = "hold"), ref_to_quarterly(x))
})

test_that("disagg(extend_with = <series>) is to_quarterly(extend_with = <series>)", {
  ref_bank()
  fx <- extsol_fx()
  x <- fx$gdppc_jp_a
  # a projection that starts in x's last year
  proj <- make_xts(2025, val = as.numeric(xts::last(x)) * 1.01^(0:6))
  colnames(proj) <- "PROJ"
  same_values(
    disagg(x, extend_with = proj),
    ref_to_quarterly(x, extend_with = proj)
  )
})

test_that("disagg(extend_with = NULL) is unchanged", {
  ref_bank()
  fx <- extsol_fx()
  x <- fx$gdppc_jp_a
  same_values(disagg(x), ref_to_quarterly(x, extend_with = NULL))
})

test_that("disagg(extend_with = 'hold') works on several series", {
  ref_bank()
  a <- make_xts(2015, val = tibble::tibble(A = 100 * 1.02^(0:10), B = 50 * 1.05^(0:10)))
  out <- disagg(a, extend_with = "hold")
  expect_equal(range(zoo::index(out)), as.Date(c("2015-01-01", "2025-10-01")))
  for (s in c("A", "B")) {
    same_values(
      out[, s],
      ref_to_quarterly(magrittr::set_colnames(a[, s], s))
    )
  }
})

test_that("interpol() without a pattern is fill_gaps()", {
  ref_bank()
  fx <- extsol_fx()
  lf <- tsbox::ts_tbl(fx$lf_us_m)
  # an explicit gap (October 2025, as in the CPS) and an implicit one
  holes <- lf |>
    dplyr::mutate(value = dplyr::if_else(time == as.Date("2025-10-01"), NA_real_, value)) |>
    dplyr::filter(time != as.Date("2023-03-01"))
  ref <- ref_fill_gaps(holes)
  new <- interpol(holes)
  expect_equal(new$time, ref$time)
  expect_equal(new$value, ref$value, tolerance = 1e-10)
  # leading and trailing gaps stay empty
  x <- make_xts("2025M1", val = c(NA, 1, NA, 3, NA))
  expect_equal(as.numeric(interpol(x)), c(NA, 1, 2, 3, NA))
})

test_that("addf() with a ramp table is addf_ramps()", {
  ref_bank()
  ramps_ext <- tibble::tribble(
    ~from, ~to, ~start, ~end,
    "2026Q3", "2027Q4", 0, -0.3,
    "2027Q1", "2030Q4", -0.1, 0.2
  )
  ramps <- dplyr::rename(ramps_ext, start = from, end = to, from = start, to = end)
  same_values(addf(ramps, per = "quarter"), ref_addf_ramps(ramps_ext))
  # no rows: zeros over the bank
  none <- addf(ramps[0, ], per = "quarter")
  expect_equal(sum(abs(none)), 0)
  expect_equal(max(zoo::index(none)), as.Date("2070-10-01"))
  expect_error(addf(ramps[, 1:3], per = "quarter"), "lacks the column")
})
