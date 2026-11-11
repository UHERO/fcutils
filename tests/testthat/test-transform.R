# Transformations on the package's example data, as they work today.

# the snapshot was recorded with index(), now index_to_base()
test_that("index() today", {
  snap(list(
    index_to_base(q_wide, 2010.1),
    index_to_base(q_wide, c(2010.1, 2010.4)),
    index_to_base(q_xts, c("2010-01-01", "2010-12-31"), 1)
  ))
})

test_that("index() is gone, so zoo::index() is not masked", {
  expect_false("index" %in% getNamespaceExports("fcutils"))
})

test_that("span() today", {
  snap(list(
    span(q_wide, 2010.1),
    span(q_wide, 2010.1, 2010.4),
    span(q_long, "2010-01-01", "2010-12-31"),
    span(q_xts, 2020.1, 2030.4, extend = TRUE)
  ))
})

test_that("span() without dates fits x to the bank", {
  withr::local_options(
    fcutils.bnk_start = as.Date("2000-01-01"),
    fcutils.bnk_end = as.Date("2040-12-31")
  )
  fitted <- span(q_xts)
  expect_equal(zoo::index(fitted)[c(1, nrow(fitted))], as.Date(c("2000-01-01", "2040-10-01")))
  expect_equal(fitted[zoo::index(q_xts)[zoo::index(q_xts) >= "2000-01-01"]], q_xts["2000/"], ignore_attr = TRUE)
  expect_true(all(is.na(fitted["2030/"])))
})

test_that("pcmp() today", {
  snap(list(
    pcmp(q_wide, 20),
    pcmp(q_wide, 4, 4),
    pcmp(q_xts, 1, 4)
  ))
})

test_that("agg() today", {
  # includes a series with a partial last year
  part <- tsbox::ts_span(q_xts, end = "2024-04-01")
  snap(list(
    agg(m_wide, conv_type = "sum", target_freq = "quarter"),
    agg(m_wide, conv_type = "uhero", target_freq = "year"),
    agg(part, target_freq = "year"),
    agg(part, target_freq = "year", na_rm = TRUE)
  ))
})

test_that("disagg() today", {
  pattern <- tsbox::ts_pick(tsbox::ts_long(m_wide), "VISNS_HI")
  snap(list(
    disagg(q_wide, conv_type = "mean", target_freq = "month"),
    disagg(a_xts, conv_type = "mean", target_freq = "quarter"),
    disagg(
      tsbox::ts_span(q_long, "2005-01-01", "2020-01-01"),
      conv_type = "mean", target_freq = "month", pattern = pattern
    )
  ))
})

test_that("interpol() today", {
  holes <- function(x, from, to) {
    dplyr::mutate(
      tsbox::ts_long(x),
      value = dplyr::if_else(time > from & time < to, NA_real_, value)
    )
  }
  inc <- holes(dplyr::filter(m_wide, time > "2002-01-01"), "2021-01-01", "2022-01-01")
  snap(list(
    quiet(interpol(inc, m_wide, extend_range = TRUE)),
    quiet(interpol(inc, m_wide)),
    quiet(interpol(inc, holes(m_wide, "2021-03-01", "2021-09-01")))
  ))
})

test_that("interpol() without a pattern fills inside gaps, linear or spline", {
  x <- make_xts("2025M1", val = c(NA, 1, 1.2, 2, NA, NA, 8, 8.5, NA))
  lin <- as.numeric(interpol(x))
  expect_equal(lin, c(NA, 1, 1.2, 2, 4, 6, 8, 8.5, NA))
  sti <- as.numeric(interpol(x, interp = "stine"))
  expect_equal(sti[c(2:4, 7:8)], c(1, 1.2, 2, 8, 8.5))
  expect_true(is.na(sti[1]) && is.na(sti[9]))
  expect_equal(sti[5:6], stinepack::stinterp(c(2, 3, 4, 7, 8), c(1, 1.2, 2, 8, 8.5), 5:6)$y)
  expect_false(isTRUE(all.equal(sti[5:6], lin[5:6])))
  expect_error(interpol(x, x, interp = "stine"), "only without a pattern")
})

test_that("chain() today", {
  early <- dplyr::filter(q_wide, time < as.Date("2000-01-01"))
  snap(list(
    quiet(chain(early, q_wide, ids = c("E_NF_HI", "ECT_HI"))),
    quiet(chain(early, q_wide)),
    quiet(chain(tsbox::ts_long(early), q_long))
  ))
})

test_that("AtoQ() and QtoA() today, including the renamed columns", {
  one <- tsbox::ts_pick(q_xts, "E_NF_HI")
  named <- function(x, nm) magrittr::set_colnames(x, nm)
  snap(list(
    quiet(QtoA(one)),
    quiet(AtoQ(QtoA(one)))
  ))
  snap_text(c(
    names(quiet(AtoQ(named(a_xts, "N_AT_JP_A")))),
    names(quiet(AtoQ(named(a_xts, "N_AT_JP_SOLA")))),
    names(quiet(QtoA(named(one, "GDP_R_AT_US_Q")))),
    names(quiet(QtoA(named(one, "GDP_R_AT_US_SOLQ"))))
  ))
})

test_that("chain() keeps names that contain a lowercase x", {
  q <- q_wide[, c("time", "E_NF_HI", "ECT_HI")]
  names(q)[3] <- "Ex_HI"
  early <- dplyr::filter(q, time < as.Date("2000-01-01"))
  out <- quiet(chain(early, q))
  expect_named(out, c("time", "E_NF_HI", "Ex_HI"))
  expect_equal(out$Ex_HI, q$Ex_HI)
})

test_that("fagg() handles a series that starts with missing values", {
  q <- tibble::tibble(
    time = seq(as.Date("2018-01-01"), by = "quarter", length.out = 16),
    A = c(NA, NA, NA, NA, 1:12),
    B = 1:16
  )
  out <- fagg(q)
  # fiscal years (July to June) labeled by the year they end in; only
  # complete years count
  expect_equal(out$time, as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")))
  expect_equal(out$B, c(mean(3:6), mean(7:10), mean(11:14)))
  expect_equal(out$A, c(NA, mean(3:6), mean(7:10)))
})

test_that("agg(partial = ) estimates an incomplete last period", {
  x <- monthly_data_example[, c("time", "VISNS_HI")] |>
    dplyr::filter(time <= as.Date("2024-11-01")) # Q4: Oct, Nov
  m <- x$VISNS_HI
  names(m) <- format(x$time)
  q3 <- mean(m[c("2024-07-01", "2024-08-01", "2024-09-01")])

  dropped <- agg(x, target_freq = "quarter")
  expect_equal(max(dropped$time), as.Date("2024-07-01"))

  avg <- agg(x, target_freq = "quarter", partial = "average")
  expect_equal(max(avg$time), as.Date("2024-10-01"))
  expect_equal(utils::tail(avg$VISNS_HI, 1), mean(m[c("2024-10-01", "2024-11-01")]))

  gr <- agg(x, target_freq = "quarter", partial = "growth")
  expect_equal(
    utils::tail(gr$VISNS_HI, 1),
    q3 * mean(m[c("2024-10-01", "2024-11-01")]) /
      mean(m[c("2024-07-01", "2024-08-01")])
  )
  # complete periods are untouched
  expect_equal(utils::head(gr, -1), dropped)
})

test_that("agg(partial = 'growth', partial_lag = '1 year') matches ptd_gr()", {
  x <- monthly_data_example[, c("time", "VISNS_HI")] |>
    dplyr::filter(time <= as.Date("2024-11-01"))
  gr <- agg(x, target_freq = "quarter", partial = "growth", partial_lag = "1 year")
  qtd <- ptd_gr(x, "quarter")
  expect_equal(
    utils::tail(tsbox::ts_pcy(gr)$VISNS_HI, 1),
    qtd$VISNS_HI[qtd$time == as.Date("2024-11-01")]
  )
})

test_that("agg(partial = ) checks its arguments", {
  x <- monthly_data_example |> dplyr::filter(time <= as.Date("2024-11-01"))
  expect_error(agg(x, "uhero", "quarter", partial = "average"), "aggregated by")
  expect_error(
    agg(x, target_freq = "quarter", partial = "growth", partial_lag = "soon"),
    "cannot read"
  )
  expect_equal(
    agg(x, target_freq = "quarter", partial = "growth", partial_lag = "1 quarter"),
    agg(x, target_freq = "quarter", partial = "growth")
  )
})

test_that("span() includes an end year or quarter in full, as p() does", {
  m <- conv_xts(monthly_data_example)
  expect_equal(nrow(span(m, 2010, 2010)), 12)
  expect_equal(nrow(span(m, 2010.1, 2010.2)), 6)
  expect_equal(span(m, 2010, 2010), m[p(2010, 2010)])
  expect_equal(nrow(span(m, 2010, "2010-03-01")), 3)
})

test_that("interpol(match = 'name') pairs series with patterns by name", {
  inc <- make_xts("2025M1", val = tibble::tibble(A = c(1, NA, NA, 4), B = c(1, NA, NA, 4)))
  pat <- make_xts("2025M1", val = tibble::tibble(B = c(1, 1, 1, 1), A = c(1, 3, 3.5, 4)))
  by_name <- quiet(interpol(inc, pat, match = "name"))
  expect_equal(as.numeric(by_name$A), c(1, 3, 3.5, 4))
  expect_equal(as.numeric(by_name$B), c(1, 2, 3, 4))
  # by position, A follows B's flat pattern
  by_pos <- quiet(interpol(inc, pat))
  expect_equal(as.numeric(by_pos$A), c(1, 2, 3, 4))
  # stems: A_Q pairs with A_M
  pat_m <- pat
  colnames(pat_m) <- c("B_M", "A_M")
  inc_q <- inc
  colnames(inc_q) <- c("A_Q", "B_Q")
  expect_equal(as.numeric(quiet(interpol(inc_q, pat_m, match = "stem"))$A_Q), c(1, 3, 3.5, 4))
  expect_error(quiet(interpol(inc, pat_m, match = "name")), "no pattern with the same name")
})

test_that("index_to_base() needs base_per", {
  expect_error(index_to_base(quarterly_data_example), "base_per")
})

test_that("agg(partial = 'hold') fills the period at the latest level", {
  x <- monthly_data_example[, c("time", "VISNS_HI")] |>
    dplyr::filter(time <= as.Date("2024-09-01")) # Jan-Sep 2024
  m <- x$VISNS_HI
  names(m) <- format(x$time)
  months_2024 <- m[format(seq(as.Date("2024-01-01"), by = "month", length.out = 9))]
  held <- mean(m[c("2024-07-01", "2024-08-01", "2024-09-01")])

  out <- agg(x, partial = "hold")
  expect_equal(max(out$time), as.Date("2024-01-01"))
  expect_equal(utils::tail(out$VISNS_HI, 1), mean(c(months_2024, rep(held, 3))))
  # complete periods are untouched
  expect_equal(utils::head(out, -1), agg(x))

  # the last observations can reach into the previous period
  jan <- dplyr::filter(x, time <= as.Date("2024-01-01"))
  held_jan <- mean(m[c("2023-11-01", "2023-12-01", "2024-01-01")])
  expect_equal(
    utils::tail(agg(jan, partial = "hold")$VISNS_HI, 1),
    mean(c(m[["2024-01-01"]], rep(held_jan, 11)))
  )
  expect_equal(
    utils::tail(agg(x, partial = "hold", partial_n = 1)$VISNS_HI, 1),
    mean(c(months_2024, rep(m[["2024-09-01"]], 3)))
  )
  # a sum adds the held values
  expect_equal(
    utils::tail(agg(x, conv_type = "sum", partial = "hold")$VISNS_HI, 1),
    sum(c(months_2024, rep(held, 3)))
  )
})

test_that("agg(partial = 'hold') needs monthly or quarterly data", {
  daily <- tibble::tibble(
    time = seq(as.Date("2024-01-01"), as.Date("2024-02-10"), by = "day"),
    value = 1
  )
  expect_error(agg(daily, target_freq = "quarter", partial = "hold"), "monthly or quarterly")
})
