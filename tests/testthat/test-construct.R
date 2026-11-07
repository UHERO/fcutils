# make_xts() and addf() as they work today (bank end from the package data,
# 2100).

test_that("make_xts() today", {
  snap(list(
    make_xts(),
    make_xts(val = 0, per = "month"),
    make_xts(start = 20100101, per = "quarter", val = 0),
    make_xts(start = 2010.1, per = "q", val = 1:10),
    make_xts(2010.1, val = 1:10),
    make_xts(start = "2010-01-01", end = "2012-12-01", per = "month", val = 1),
    make_xts(
      start = 201001, per = "q",
      val = tibble::tibble(E_NF_HON = 1:10, ECT_HI = 11:20)
    )
  ))
})

test_that("addf() today", {
  snap(list(
    addf(),
    addf(201002, 201504, 1, 2),
    addf(20100101, 20601201, 1, 2, per = "month"),
    addf(20100101, from = 1, to = 2, per = "quarter"),
    addf(2010.2, 2015.4, 1, 2, "ECT_HI")
  ))
})
