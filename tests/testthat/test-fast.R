# The fast paths in R/fast.R must give exactly what the tsbox calls they
# replace give (rows, values, class), and fall back to tsbox otherwise.
# Random series: monthly, quarterly, annual, weekly, daily; one or several;
# missing values at the start, end or inside; zeros; all missing.

random_case <- function() {
  freq <- sample(c("month", "quarter", "year", "week", "day"), 1, prob = c(.35, .35, .2, .05, .05))
  ids <- sample(c("Z", "A", "M_HI", "B2"), sample(c(1, 1, 2, 3), 1))
  start <- if (freq %in% c("month", "quarter", "year")) {
    as.Date(sprintf("%d-%02d-01", sample(2000:2005, 1), sample(c(1, 4, 7, 10), 1)))
  } else {
    as.Date("2020-01-06")
  }
  x <- dplyr::bind_rows(lapply(ids, function(i) {
    n <- sample(c(1, 2, 5, 13, 30), 1)
    v <- round(stats::rnorm(n, 10, 5), 2)
    pat <- sample(c("none", "lead", "trail", "mid", "zero", "allna"), 1)
    if (pat == "lead") v[seq_len(min(n, sample(1:5, 1)))] <- NA
    if (pat == "trail") v[max(1, n - sample(0:4, 1)):n] <- NA
    if (pat == "mid" && n > 3) v[sample(2:(n - 1), min(n - 2, 3))] <- NA
    if (pat == "zero") v[sample(n, min(n, 3))] <- 0
    if (pat == "allna") v[] <- NA
    tibble::tibble(id = i, time = seq(start, by = freq, length.out = n), value = v)
  }))
  if (length(ids) == 1 && stats::runif(1) < 0.5) x <- dplyr::select(x, -"id")
  x
}

result <- function(expr) {
  tryCatch(suppressMessages(expr), error = function(e) "error")
}

test_that("lag_ratio(), pcy() and frequency_fast() equal tsbox", {
  withr::local_seed(20261107)
  for (i in 1:200) {
    x <- random_case()
    xl <- conv_long(x)
    k <- sample(list(1, 2, 4, 12, "1 year", "3 months"), 1)[[1]]
    expect_identical(result(lag_ratio(xl, k)), result(xl %ts/% tsbox::ts_lag(xl, k)))
    expect_identical(result(pcy(xl)), result(tsbox::ts_pcy(xl)))
    to <- sample(c("year", "quarter"), 1)
    ag <- sample(c("mean", "sum"), 1)
    nr <- sample(c(TRUE, FALSE), 1)
    for (inp in list(x, xl)) {
      expect_identical(
        result(frequency_fast(inp, to = to, aggregate = ag, na.rm = nr)),
        result(tsbox::ts_frequency(inp, to = to, aggregate = ag, na.rm = nr))
      )
    }
  }
})

test_that("the fast paths are taken where they should be", {
  q <- conv_long(quarterly_data_example)
  expect_equal(lag_periods(q, 4), 4L)
  expect_equal(lag_periods(q, "1 year"), 4L)
  expect_equal(month_step(q), 3L)
  gap <- q[-5, ]
  expect_true(is.na(lag_periods(gap, 4)))
  expect_true(is.na(lag_periods(q, "2 weeks")))
})
