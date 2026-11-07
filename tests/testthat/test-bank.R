test_that("make_xts() and addf() follow the bank options", {
  withr::with_options(
    list(fcutils.bnk_start = as.Date("2000-01-01"), fcutils.bnk_end = as.Date("2070-12-31")),
    {
      x <- make_xts(val = 0, per = "quarter")
      expect_equal(range(zoo::index(x)), as.Date(c("2000-01-01", "2070-10-01")))
      a <- addf(2010.1, 2011.4, 1, 2)
      expect_equal(range(zoo::index(a)), as.Date(c("2000-01-01", "2070-10-01")))
      expect_equal(as.numeric(a["2011-10-01"]), 2)
    }
  )
})

test_that("without a bank, functions that need one say how to set it", {
  withr::with_options(
    list(fcutils.bnk_start = NULL, fcutils.bnk_end = NULL),
    {
      expect_error(make_xts(val = 0), "No bank set")
      expect_error(addf(2010.1, 2011.4, 1, 2), "No bank set")
      # a series with its own dates needs no bank
      expect_equal(nrow(make_xts(2010.1, val = 1:4)), 4)
    }
  )
})
