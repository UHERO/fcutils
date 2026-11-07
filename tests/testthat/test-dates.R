# Record of today's date parsing (step 1 of the plan in forecastr's
# notes/EXTSOL_GUIDE.qmd, section "Candidates for fcutils"). Some of these
# results are wrong and will change in step 2; the snapshot shows which.

date_inputs <- list(
  2019, "2019", 2019.4, "2019.4", "2019Q4", "2019q4", 2019.5, 2019.12,
  "2019M10", "2019M3", "2026M03", "2019-10", "2019-10-15", 20191015,
  202002, 201912
)

test_that("to_ymd() today", {
  out <- vapply(
    date_inputs,
    function(x) as.character(suppressWarnings(to_ymd(x))),
    character(1)
  )
  names(out) <- vapply(date_inputs, as.character, character(1))
  snap_text(out)
})

test_that("p(), pq(), pm(), py() today", {
  snap_text(c(
    p("2010-01-01", "2020-01-01"),
    p(20100101, 20200101),
    p(2010.1, 2020.4),
    p(, 2020.4),
    p("2010Q1", "2020Q4"),
    p(2010, 2020),
    p(2019.4, 2026),
    pq("2010Q1", "2020Q4"),
    pq(2010.1, 2020.4),
    pm("2010M1", "2020M4"),
    pm(2010.1, 2020.4),
    py(2010, 2020)
  ))
})

test_that("nmons() and nqtrs() today", {
  # nmons(2010.1, 2010.4) is 10: to_ymd() reads 2010.4 as a quarter, while
  # the documentation means April
  snap_text(c(
    nmons("2010M1", "2010M2"),
    nmons(2010.1, 2010.4),
    nmons("2010-01-15", "2010-04-15"),
    nqtrs("2010Q1", "2020Q4"),
    nqtrs(2010.1, 2020.4),
    nqtrs("2010-02-01", "2020-11-01")
  ))
})
