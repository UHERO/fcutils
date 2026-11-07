test_that("make_xts() and addf() follow the bank options", {
  expect_equal(max(zoo::index(make_xts(val = 0))), as.Date("2100-01-01"))
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
