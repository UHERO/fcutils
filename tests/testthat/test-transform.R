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
