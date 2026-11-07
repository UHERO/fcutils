# Transformations on the package's example data, as they work today.

# the snapshot was recorded with index(), now ts_rebase()
test_that("index() today", {
  snap(list(
    ts_rebase(q_wide, 2010.1),
    ts_rebase(q_wide, c(2010.1, 2010.4)),
    ts_rebase(q_xts, c("2010-01-01", "2010-12-31"), 1)
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

test_that("pcmp() today", {
  snap(list(
    pcmp(q_wide, 20),
    pcmp(q_wide, 4, 4),
    pcmp(q_xts, 1, 4)
  ))
})

test_that("aggr() today", {
  # includes a series with a partial last year
  part <- tsbox::ts_span(q_xts, end = "2024-04-01")
  snap(list(
    aggr(m_wide, conv_type = "sum", target_freq = "quarter"),
    aggr(m_wide, conv_type = "uhero", target_freq = "year"),
    aggr(part, target_freq = "year"),
    aggr(part, target_freq = "year", na_rm = TRUE)
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

test_that("multi_chain() today", {
  early <- dplyr::filter(q_wide, time < as.Date("2000-01-01"))
  snap(list(
    quiet(multi_chain(early, q_wide, ids = c("E_NF_HI", "ECT_HI"))),
    quiet(multi_chain(early, q_wide)),
    quiet(multi_chain(tsbox::ts_long(early), q_long))
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

test_that("multi_chain() keeps names that contain a lowercase x", {
  q <- q_wide[, c("time", "E_NF_HI", "ECT_HI")]
  names(q)[3] <- "Ex_HI"
  early <- dplyr::filter(q, time < as.Date("2000-01-01"))
  out <- quiet(multi_chain(early, q))
  expect_named(out, c("time", "E_NF_HI", "Ex_HI"))
  expect_equal(out$Ex_HI, q$Ex_HI)
})
